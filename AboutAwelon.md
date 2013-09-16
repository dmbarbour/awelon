
# Awelon

Awelon is a richly typed language for reactive demand programming (RDP). RDP is a general purpose, reactive dataflow model that addresses challenges for developing in open, distributed, heterogeneous systems (see AboutRDP for more). Awelon is intended to be a usable programming language for humans, a feasible distribution language for RDP code and software components, and a viable target for RDP development environments (some of which might be more visual in nature).

In Awelon, an application or component can describe code that executes integrates with distributed services, or executes in multiple OS level processes.

## Tacit Programming

Awelon code is [tacit](http://en.wikipedia.org/wiki/Tacit_programming). Juxtaposition is composition. All inputs are implicit, in an environment. However, unlike most tacit languages, Awelon's environment exists only as a compile-time type, and all manipulations to it are pure. Developers use the environment at a *conceptual* level - a way to organize resources and workflows, and to integrate them. 

At runtime, the environment is gone and the data plumbing is compiled completely away.

### Stack Based Programming

Like many tacit programming languages, Awelon supports stack-based programming. One of the key aspects of tacit programming is how *literals* are handled - e.g. if a developer writes `10 11 12`, what happens? Somehow, those literals must be added to the environment. For example, we can push literals onto a stack.

        42 %  e -> (Number * e)
        12.3 %  e -> (Number * e), exact 123/10
        "literal string" %  e -> (Text * e)
        [swap] %  e -> (Block * e)

In Awelon, literals are simply numbers (rationals or integers), text, or blocks of code. Developers often choose to distinguish rational numbers from integrals (e.g. by testing that the number is integral), but Awelon lumps them together. Also, due to ad-hoc polymorphism, Awelon essentially treats static blocks as untyped until they're applied. 

Literals have 'Static' types, meaning their values are available at compile-time both for operations and decisions. Static types also are the key ingredient to make tacit programming work with an arrowized model, enabling us to model 'first' without use of explicit parameters.

        %  first :: (x -> x') -> ((x*y) -> (x'*y))     HASKELL
        %  first :: ((x -> x') * (x * y)) -> (x' * y)  AWELON
        [swap] first %  ((a*b)*c) -> ((b*a)*c)

Support for partial application with 'first' is valuable for both code reuse and expressiveness. Awelon's model for 'first' has greater expressiveness because it allows the specification of the behavior to be cleanly separated from its call site, thus avoiding the need for an orthogonal abstraction model (such as lambdas). There is a price: Awelon's first is expressive enough to cause trouble. The traditional non-terminating lambda (`(\x->(x x) \x->(x x)`) can be represented (`[copy apply] [copy apply] first`).

To help illustrate what tacit code often looks like, I'll define a few more functions that assume a single stack:

        % Assuming available behaviors:
        %  assocl :: (x * (y * z)) -> ((x * y) * z)         
        %  swap   :: (x * y) -> (y * x)                     
        %  intro1 :: x -> (1 * x)      `1` here means the unit type                  
        %  elim1  :: (1 * x) -> x                        
        %  copy   :: x -> (x * x)                           
        
        apply  = [intro1 swap] second first swap elim1      %  ((x -> x') * x) -> x'
        second = swap [swap] first swap first swap          %  ((y -> y') * (x * y)) -> (x * y')
        assocr = swap assocl swap assocl swap               %  ((x * y) * z) -> (x * (y * z))
        roll2  = assocl [swap] first assocr                 %  (x * (y * z)) -> (y * (x * z))
        roll3  = [roll2] second roll2                       %  (a * (b * (c * d)))) -> (c * (a * (b * d)))
        roll4  = [roll3] second roll2
        dup    = [copy] first assocr                        %  (a * e) -> (a * (a * e))
        pick1  = [dup] second roll2                         %  (a * (b * e)) -> (b * (a * (b * e)))
        pick2  = [pick1] second roll2
        %  Note: the above definitions are for illustrations; those
        %  in use may be different (e.

Tacit code is easy to incrementally parse and process, and similarly easy to generate. Those are useful properties for Awelon's goals as a distribution or target language. Tacit code is naturally composable, and should be effective for representing software components. Avoiding direct manipulation by names enables many of RDP's invariants to be enforced constructively. Tacit code has very little syntactic noise. 

However, tacit code has a weakness: much of the data plumbing becomes explicit. Data plumbing words become a form of semantic noise, unrelated to the problem domain. For productive development, it is necessary that these data plumbing operations become simple, intuitive, fading into the background of the developer's attentions. 

*ASIDE:* Traditional languages also have difficulty with data plumbing - it just happens in higher layers: concurrency, callbacks, collections. RDP is designed to address many challenges in those higher layers.

### Awelon's Extended Environment

A single stack environment can feel cramped. It can be painful to thread common objects through the stack - e.g. singletons, authorities. The stack is often a poor place for large-scale configuration variables. Also, while a stack works reasonably well for a sequential task, it's a poor fit for a larger system with concurrent tasks and workflows.

To address this, Awelon assumes an extended environment.

        (stack * env)

Critically, literals (blocks, numbers, text) are now pushed onto the inner stack, ensuring the rest of the environment remains accessible in a fixed location. This provides a great deal of freedom for modeling the larger environment. 

The standard environment (i.e. the one used in the standard libraries) is as follows:

        (stack * (hand * (powerblock * (stackName * namedStacks))))

The stack enables the normal stack operations (like `roll` and `dup`). All literals are added to the stack, and the standard library treats the stack as the active area. The hand is a secondary stack that serves as temporary storage and a semantic clipboard: developers can use `take` and `put` to move objects from the stack to the hand, and `juggle` to roll items in the hand. 

The powerblock is an important block: it contains capabilities to observe and influence the outside world. By placing this important block in a prominent location, library functions can easily access it, almost as if Awelon had ambient authority. Subprograms often run within a named fragment of the powerblock, created by forking it.

Finally, the standard environment provides a list of named stacks. Programmers can use `"foo" store` and `"foo" load` to operate on a named stack like a global variable.  And programmers may `"foo" goto` a stack by name, storing away the current stack. Navigating between stacks can be useful when modeling concurrent workflows, the hands provide some implicit data plumbing to help programs interact.

*NOTE:* Reinterpreting the structure of the environment has serious overhead: a lot of libraries may need to change. However, Awelon is designed for component-based software, which often require very few arguments. Software designed as components is often reusable regardless of which environment they use internally. Libraries naturally assume more about their environment. 

### Conditionals and Choices

Choice in RDP is modeled using sum types `(x + y)`. Any decision results in a sum type (and even a boolean value is modeled as a choice of units. Developers then operate upon that sum type, keeping it open for extension. Only one path will be active at a time (up to minor variations in latency). Developers might think of this as a switching network, especially since `x` and `y` may correspond to different locations. 

In addition to basic data plumbing (intro0, elim0, mirror, assocl+, rot3+) and combinator (left), there are a few other operations for interactions between products and sums.

        disjoin :: (x@p * ((e@p*y) + z)) -> ((x * y) + (x * z))  
        distrib :: ((a * b) + (c * d)) -> ((a + c) * (b + d))
        conjoin :: ((x * y) + (x * z)) -> x * (y + z)
        merge   :: (x+x) -> x

Here the `@p` on disjoin is to represent that disjoin requires knowledge that a value with evidence of the split will be in the same location as a value outside of it - i.e. we cannot make a choice in one location then apply it somewhere else without some evidence of that choice. 

The distrib primitive is mostly used as the final step for copying a sum type.

Awelon doesn't strictly enforce equivalent types on merge. But the types must be compatible; all operations on a sum after a merge are applied on both branches. A physical merge will often be favored for performance. 

## Awelon's Module System

Awelon has a very simple module system. Essentially, a module consists of one import line followed by one or more definition lines. An import line contains a comma separated list of module names. The association between a module name and a module definition is externally determined (e.g. by file name). Module names must be simple words, i.e. valid as both Awelon export words and filesystem/URL components. 

        import common, packageWithLongName AS p, foo
        this = [f] _s
        f = foo [p:bar foo:bar] ap %  here 'foo' means 'foo:this'
        _s = assocl swap rot2 first swap
        % TODO: replace this with a usable module...

All words defined in a module are exported from it, except those prefixed with the underscore character such as _s above. (Imported words are not re-exported.) All words are also available prefixed with 'module:' from which they come. Words may be defined in any order within a module. Imports may be cyclic. However, *definitions* may not be cyclic or recursive, nor may they even appear to be. If developers wish to re-export a word, they must use the prefixed form on the right hand side, e.g. `bar = p:bar`. Also, if there is any ambiguity in a word's definition (i.e. after expansion to primitives), the prefixed form must be used.

The word 'this' has special meaning. Within a module, 'this:' may be used as the prefix to disambiguate a module's own words (this is only necessary within a definition). When imported from another module, the word 'this' is mapped instead to the import name, e.g. 'foo' will mean 'foo:this'. Awelon is intended for component based software; I believe Awelon's use of 'this' has nice connotations to encourage treating modules as software components.

Awelon's syntax is very simple. Imports and new definition must start at the beginning of a new line. If a definition extends for multiple lines, there must be spacing near the front. Code itself is just a space-separated sequence of words and literals. Awelon has simple line comments starting with `%`. 

There are no default words or imports in Awelon.

Only literals can be used without an import. If there is something like a common prelude or standard environment, it must be on the import line. Consequently, it is not difficult for developers to create alternative programming environments (the only requirement is that literals are added to the first element of a pair).

*NOTE:* I believe developers shouldn't need more than a line of boiler plate before they get to useful work, which is why I allow multiple imports on one line. I discourage hierarchical module systems. Libraries should generally be exported at the package layer. Ambiguity is rare in practice, and resolved easily enough. Modules intended to serve as software components are encouraged to export 'this' and little else (perhaps authoring info, etc.).

### Application Model

An application in Awelon is identified by naming a module. The 'this' value of that module is selected as the application behavior. An Awelon application has only one input: the powerblock. The powerblock is a linear-typed block of code that serves as a source of capabilities - access to the outside environment and any shared spaces. 

The internal definition of the powerblock is not standardized. More precisely, it will depend on de-facto standards. However, a powerblock can contain enough logic to process and translate many requests intelligently. 

In general, there are a few 'stages' involved with the use of any capability. For example, a two-stage request may involve asking the powerblock for a capability (another block), then using the capability to observe something about the outside world. A three-stage request might involve negotiating a long-term relationship with the capability. Staging is a powerful basis for stability, for controlling where recomputations occur and how much of the program 'breaks' due to changes in observable conditions.

A "well-written" Awelon program should be stable to most changes and adaptable to many more. It is the job of the application developer to perform a mix of: (a) modifying the powerblock to meet their expectations (e.g. remapping names, specifying preferences), (b) modifying their expectations to match the resources in the powerblock (e.g. performing conditional tests and making decisions, using fallbacks).

(*NOTE:* When I say the powerblock is the only input, I mean that literally. In general, the first word of an application will wrap the powerblock into an environment - perhaps Awelon's standard environment, perhaps a rich application framework, perhaps a testing framework. This first word can help clarify the nature of the application to developers.)

### Automatic Testing

Tests are possible by convention. Developers define words starting with 'test' or '_test'. These are considered applications, though they will run in an isolated environment. One generally will use an error capability to indicate failure, or potentially a warning if there is an issue. *NOTE:* Test words are normal words, just interpreted and processed by convention. 

### Awelon Virtual Machine

The Awelon Virtual Machine (AVM) is a description of an RDP-based programming environment. 

This description includes types, partitions, and behavior primitives. It is a complete description, repeating even the ‘standard’ primitives like first and compose. Unlike a traditional VM, an AVM can describe heterogeneous systems, where different partitions have different behavior primitives and asymmetric communication. An AVM may additionally include: equational laws, proposed rewrite rules, and other properties. An application model would describe behavior types for applications or pluggable applets, along with some English text about how it is used. 

Within the avm, the type of a behavior is represented as a block to execute on a type descriptor. The 

The AVM is represented as an Awelon module that operates in a restricted subset of Awelon to construct its description (i.e. as opposed to using JSON or XML). This restricted subset is provided by a module called 'avmboot'. The AVM may import avmboot or other modules that ultimately depend on avmboot (rather than avmprim). The parser must understand avmboot implicitly during the bootstrap process. It provides just a few primitive operations.

Essentially, the AVM is a machine-processed standards document. There won't be many AVMs (probably the main line, a few experimental branches, maybe a didactic subset). After Awelon matures, the AVM will evolve very slowly. 

An Awelon application is limited to one AVM. Often, which AVM will be implicit in the development environment, and a compatible AVM can be switched in transparently. (An incompatible AVM can also be switched in; it just breaks your code.) In general, compilers will be limited to a single evolving line of AVMs.

### Primitive Words

A special module called 'avmprim' is created automatically from the AVM. This contains all the primitive words defined in the AVM, and is the ultimate root of all modules in an Awelon project. The module 'avmboot' contains a subset of primitive words, which may be used by the avm (or any module the avm uses).

In general, developers do not directly import the primitives module. Primitives are too far removed from the more complicated multi-stack environments presented to developers. Even if they want low-level primitives, developers should use a module that extends them with all the common helpers (which I might name 'pure').

### "Awelon Object"

Modules in Awelon are also called Awelon Objects or AOs. The extension **.ao** should be used for modules represented in the filesystem (though the plan is for Awelon to be expressed in a more wiki-like environment). AO files should be encoded in UTF-8. The name of the module is simply the filename excluding the extension. If there are ambiguous filenames (e.g. with installed packages), they must be resolved externally.

## Metaprogramming via Ad-hoc Polymorphism

Awelon supports ad-hoc polymorphism by introspection of types and static values. For example, developers can examine a type to see whether it is a product.

        % testIsProduct :: x -> (x=(a*b)|x)  ... conceptually

Introspections can also test numbers, compare texts, and result in static choices. Developers can take different actions on different branches, may assert certain properties, then may 'forget' which branch they were on - even if the choices result in different types.

A good example where this is useful is to create a generic copy or drop operator. Awelon does not support copy or drop generally because affine types cannot be copied, and relevant types cannot be dropped, and sealed types cannot even be introspected (beyond determining their owner). 

As noted earlier, Awelon is capable of modeling a fixpoint combinator, and so can represent these decisions recursively - e.g. to build a copy or drop operation, or to look up a stack by name, in a list. 

Combined with fixpoint combinators, developers can write recursive compile-time functions - e.g. to build a recursive copy or drop operation that works unless there is a linear type or sealed value preventing it. 


### Recursion and Repetition

A common situation, as with 'copy', is that developers need to operate on deep structures. For basic tree structures, we can take advantage that the type of an Awelon program is always finite. A simple recursion combinator for a tree might look like:

        mapTree :: OnLeaf * JoinTrees * Trees -> whatever

Of course, we may want more complicated combinators: folds, searches, etc.. Awelon doesn't build in any particular combinators, and instead leverages its compile time expressiveness. Awelon is Turing complete:

        (λx.(x x) λx.(x x)) %  "Forever" in Lambda Calculus.
        [copy apply] [copy apply] apply %  "Forever" in Awelon

Anyhow, Awelon must use a [fixpoint combinator](http://en.wikipedia.org/wiki/Fixed-point_combinator) and builds a library of recursive combinators atop it.

        %  a couple call-by-value fixpoint combinators from Curry and Turing
        Yv = λf. (λx.f (λv.((x x) v))) (λx.f (λv.((x x) v)))
        Θv = (λx. λy. (y (λz. x x y z))) (λx. λy. (y (λz. x x y z)))

At some point, I'll need to convert those into the associated Awelon behaviors. It can be done, I'm certain. But, right now, they make my head swim.

### Support for Sloppy Programming

There is a valuable role in this wide world for *sloppy* programming: rapid prototyping and executable pseudocode, plug-and-pray integration efforts, auto-configuration against available features, and live programming environments where active developers can easily gain feedback and refine on-the-fly.

Awelon's ad-hoc polymorphism should make a fair level of sloppiness feasible in terms of "smart" wiring logic and glue words, and leveraging latent choice extensively. The cost of sloppiness is a more expensive compile-time computation, since developers are pushing more work to the compiler.

### Aspect Oriented Awelon

A potential feature leveraging ad-hoc polymorphism is to model [aspect-oriented programming](http://en.wikipedia.org/wiki/Aspect-oriented_programming) in the Awelon compile-time. Basically, developers can model join-points using explicit `"foo" signal` code (to mark a "foo" join point). The 'signal' operation can look up the current advice (a block) associated with "foo" and effectively insert it in place of the signal. AOP can be very expressive.


## Awelon's Types and Safety

Awelon is richly typed, though not in a conventional manner. Developers cannot define new types. But they can construct anonymous, ad-hoc types by composing simple values. And there are capabilities to share these types with other applications. 

Similarly, Awelon's typing does not fit the conventional classification of static vs. dynamic. Developers cannot declare types for behaviors, and there is no implicit coercion of types. A compiler can in many cases detect statically whether assumptions are violated. A program can introspect structures - adapt to them or validate them.

RDP has powerful features to help control failure even at runtime:

1. communication continuous and reactive, easy to trace and stop
2. logical time and anticipation; stop a program before it causes problems
3. implicit membrane and revocation of capabilities

If there are errors the program can logically be removed just before the errors occurred. If errors happen in early setup, then it can be as if the app was never installed. Conversely, an RDP system can continue to attempt to reinstall the app over time (when the problem was external) and may even probe for remembered problem conditions more precisely to keep it cheap.

Awelon goes a few steps further. 

When programmers don't trust a particular subprogram, they can grab a unique 'executive' capability from their powerbox, then run a block under the executive. An executive separates failure of the inner block from failure of the outer. Since the executive is a capability (not a universal primitive) developers can more effectively control and audit the use of this feature, and gain a better understanding of the 'health' of a subprogram.

Conceptually, every Awelon application runs under control of an 'executive' within an even larger RDP system. Awelon applications can interact as if they were all subprograms of a larger application. 

Awelon's basic types are:

* atomic types: numbers, text, blocks
* structural types: products, sums
* substructural types: affine, relevant, linear
* barrier type: based on sealer/unsealer pairs
* temporal type: latency, latency bounds 
* spatial type: logical locations

Also, since Awelon lacks ambient authority (all authority is through blocks) many problems can be addressed effectively by object capability security patterns and controlling distribution of authority. 

### Substructural Types

Blocks in Awelon may arbitrarily be marked `noDrop` and/or `noCopy`. These correspond to the basic substructural types: affine (no copy), relevant (no drop), linear (no drop, no copy). A new literal block may be dropped or copied. 

Only blocks can be so marked, but that's sufficient for any purpose. A block is properly 'consumed' by first, left, or an executive. However, a 'consumed' block can still output more blocks (including an updated copy of itself) as output. 

Substructural types are very useful for certain forms of reasoning - e.g. handshakes and multi-step protocols, uniqueness and exclusivity, limited resources and fan-in, and modeling responsibilities or requirements. 

### Uniqueness Source

The uniqueness source is a static object that represents a source of "unique values". Unique values include GUIDs, exclusive state, sealer/unsealer pairs. In Awelon, the powerblock - one of the two initial arguments to every application - serves also as the uniqueness source. (This coupling is convenient.)

A uniqueness source cannot be copied, because if it were copied it would no longer be unique. A uniqueness source can be split, such that we have two or more distinct uniqueness sources, which is functionally similar to a copy. 

When providing exclusive state, it is very useful to have a stable identity for that state: it simplifies orthogonal persistence, live programming, runtime upgrade, dynamic behavior. An anonymous split (left/right) split will be very unstable to code changes that rearrange the order in which splits occurr or introduce extra splits upstream. 

For stability, Awelon requires that each split action - for both the uniqueness source and the individual unique values - utilize a text identifier. The text becomes the source of stability, similar in nature to a directory structure in a filesystem. The split generates a 'child' object, and a 'parent'. The parent is (statically) unable to reuse the child identifier.

Since the child has a fresh set of identifiers, it can be passed into an independently developed software component without risk of namespace clashes. Meanwhile, the parent is kept around for further divisions.

### Sealer/Unsealer Pairs

A sealer/unsealer pair can be created from the powerblock:

        newSealerUnsealerPair :: (Text * Power) -> ((Sealer Unique * Unsealer Unique) * Power')
        type Sealer u = x -> Sealed u x
        type Unsealer u = Sealed u x -> x

Sealers and unsealers are just static blocks. They can be copied or dropped as normal. However, they are uniquely paired: the only means to unseal a value is to have a copy of the corresponding unsealer. The text associated with the sealer/unsealer pair is there mostly for debugging and error messages.

Sealer/unsealer pairs can be used in a number of creative ways. They can enforce parametricity or security properties. They can express rights-amplification patterns. They can model identity and blame (cf. [Horton's Who Done It](http://www.erights.org/elib/capability/horton/)). They can also model first-class abstract data types or existentials. 

In most use cases, sealer/unsealer pairs can be completely eliminated before runtime. However, in a distributed system, sealed values can guide encryption decisions for specific signals (orthogonal to transport-layer encryption). In particular, if sending information to a host that the compiler doesn't statically know will possess the unsealer, it may be worth encrypting the value. 

*Note:* Sealed values are implicitly linear. They must be unsealed before they can be dropped or copied.

### Abstract Data Types and Existentials

Awelon's introspection sometimes interferes with modularity, implementation-hiding, and user-defined types. However, idiomatic use of sealer/unsealer pairs can model a first-class module system, complete with ADTs or existentials. How this works:

1. The hidden type (an ad-hoc sum or product) is modeled by use of a sealer
2. A record of functions (product of blocks) is formulated to operate on the hidden type.
3. Each function is wrapped with the unseal/seal operations for the hidden type.
4. For ADTs, the record of functions is held separate from the sealed type
5. For existentials, the record of functions is coupled to each instance of the type

That's it. The sort of implementation hiding performed by most module systems is quite simple. Though, API designers must explicitly include the drop and copy methods if they don't want their new type to be linear.

In Awelon, these ADTs would implicitly be reactive, but would have relatively static structure.

### Dynamic Collections

Any general purpose programming language should have effective support for collections - lists or arrays - of both static and dynamic sizes. This is especially true for a reactive model, where there is a big difference between "a signal of arrays" (where the array updates atomically) and "an array of signals" (where each element may update independently). In Awelon, arrays and matrices of static size can be modeled by clever use of products and introspection; I'll leave those to libraries. 

However, primitive support is required for efficient, dynamic collections in Awelon.

I am not entirely sure how I will approach this yet. The basic thoughts however are:

* vectors and sets are very promising base collections
* perhaps a 'unique-map', i.e. a set indexed by a unique value per signal

The dynamic collection types will have homogeneous type.


Awelon will provide two dynamic collection types: vectors and sets of homogeneous type (which also means all static values must be the same). These collections will be processed by collection-oriented primitives, e.g. foreach operations, or folds for vectors. Vectors are the more expressive, having set operations plus support for order-dependent operations. 

*Desiderata:* supporting vectors with coupled dimensionality. Knowing two vectors have the same size, even if I don't know what is that size, would be very useful for some analyses. Also, I am hoping to avoid 'indexing' into vectors. 

### Dynamic Behaviors

Dynamic behaviors are, relatively, a second-class feature in Awelon and RDP. They operate under several constraints in order that they can be effectively implemented:

* cannot output static choices (dynamic choice OK)
* other static outputs must exactly match a template
* inputs are synchronized and start in one partition
* rigid structure for output types and latency
* cannot input or output fractional or negative types

Despite these limitations, dynamic behaviors are useful for a wide variety of applications. They can model runtime resource or service discovery, plugins and extensions, authorities and capabilities, staged programming or metaprogramming, code distribution, or live update. Awelon application models will generally fit the dynamic behavior constraints.

I'm still deciding some aspects of this; dynamic behaviors might need some extra support to readily enforce static type safety. 


## Miscellaneous

What follows are ideas and potential idioms for Awelon users.

### Multi-Line Text

Awelon does not directly support multi-line text. However, developers can get the equivalent of multi-line text by using some sort of sentinel then using Awelon's static behaviors to construct the larger text:

        author = "William Wordsworth"
        daffo2 = linesStart
            "Continuous as the stars that shine"
            "And twinkle on the milky way,"
            "They stretched in never-ending line"
            "Along the margin of a bay:"
            "Ten thousand saw I at a glance,"
            "Tossing their heads in sprightly dance."
            linesEnd

Here, `linesStart` would place an obvious sentinel on the stack, and each line would then add to the stack. The `linesEnd` word would then introspect the stack, append the lines, and remove the sentinel, leaving the constructed text. This would occur at compile-time. (Developers aren't restricted to text literals, of course.)

### Block-Free Data Plumbing

Awelon's complex environment has a consequence: blocks are no longer added in the right location for immediate use by 'first'. To address this issue, Awelon needs to extract the block for application, along the target inputs. This requires some expressive 'block-free' data plumbing. To support this, a new primitive is introduced:

        rot3 :: (a*(b*(c*d))) -> (c*(a*(b*d)))

Between intro1, elim1, assocl, swap, and rot3, any parametric data plumbing can be expressed block-free. This enables 'first' to be used on targets in the stack or hand... albeit after sufficient environment manipulations. A few examples of block-free definitions:

        rot2 = intro1 rot3 intro1 rot3 elim1 elim1  % (a * (b * c)) -> (b * (a * c))
        rot4 = assocl rot3 rot2 assocr rot3         % (a * (b * (c * (d * e)))) -> (d * (a * (b * (c * e))))
        rot5 = assocl rot4 rot2 assocr rot3         % etc...
        zip2 = assocr rot3 rot2 assocl              % ((a * b) * (c * d)) -> ((a * c) * (b * d))
        take = zip2 rot2                            % ((x * s) * (h * e)) -> (s * ((x * h) * e))
        put  = rot2 zip2                            % (s * ((x * h) * ee)) -> ((x * s) * (h * ee))
        reifyStack =  intro1 rot2 assocl            % (s * e) -> ((s * unit) * e) - stack becomes object on stack

The zipper operations needed to manipulate document-like structures are also block-free. 

### Documents, Databases, Diagrams, and DSLs

In Awelon, the type system is capable of representing some very ad-hoc structures, potentially including document-like structures, diagrams, game worlds, higher order applications, simple logic databases, even module systems. In general, developers can represent DSLs within the type system, then use words to transform or interpret them.

To help with this use-case, Awelon supports a 'zipper' concept for the top object on the current stack. Developers can easily 'enter' a structure, and navigate through as though with a cursor. Programmers can pick items up, move or modify them, put them down; they can also navigate to other documents to perform transclusions, or make promises in one part of a document and carry them to another to wire up parts of a single document.

This technique is a reasonable basis for actually programming a wide variety of document-like applications. E.g. if we're going to create an HTML page, we can model the static structure in the type system, and hook some data together within the document. The main alternative is to use an API, but a mix is quite feasible - using the API for dynamic structures within a rigid template.

### Agent Resources

An "agent resource" is a behavior - representing a service or software agent - that becomes active 'on demand', i.e. whenever at least one subprogram demands it. There are two reasons to use agent resources:

* the behavior contains affine types or unique bindings to state
* for efficiency, to avoid unnecessary redundancy of behavior

An agent resource is activated by a simple unit signal `Unit@p` for some partition p that supports agent resources. Unit signals from multiple sources can easily be merged together, to keep the agent resource alive for as long as necessary. 

        newAgentResource :: (U! * Text) * (B * N) -> (U!' * A)

To construct an agent resource takes four arguments:

* a static uniqueness source and a Text value indicating the unique child
* a behavior that will receive its unique source and a `Unit@p` signal.
* a number: how long the agent will remain active after demands are gone

The uniqueness source and text gives every agent resource a stable name and stable subdirectory for its own state resources. The agent behavior cannot have any substructural type (not affine, not relevance); logically, the agent is external (super-structural). The extra delay after demands are eliminated can help 'smooth over' cases where the agent would otherwise be bouncing too quickly between active and inactive; it can also provide a grace period for explicit cleanup. But it probably shouldn't be longer than a few seconds. 

The result is the parent unique source and the agent, which is simply a block. 



### Debugging Primitives

Awelon developers can emit a compile-time error or warning if they need one.

        error :: (Text*x) -> x
        warn  :: (Text*x) -> x

A warning or error will apply if 'x' is ultimately on a live branch of a static computation. The given text can be computed statically; in addition, source location and type info will be provided. 


### Stable Stateless Models

### Exponential Decay State


### Debugging

Errors, Warnings, Traces


### IDE Integration

translating drag and drop to words
automatic rewriting to simplify the program

Rendering Hints (relative positions?)

Gauges (some sort of: "if the user is near, install this here" or "when there's someone in this forest, here's the sound to make")

Diagrams-as-Data (idea: like Pure Data support diagrams as a form of input data, representing diagrams in type-system with static values).

Gauges 

Gauges? Maybe some way to say "hey, display this using XYZ, but only if we're looking at it"

Due to its support for static types, Awelon code can readily be augmented with 'active' documentation - e.g. commands that don't do anything except indicate how to display or debug the code itself. 

## Future Development or Wishlists

### Dependent Types

Awelon's static phase essentially supports values in the type-system and supports metaprogramming. But Awelon does not have any support for dependent types at runtime, e.g. I cannot assert that a pair of dynamic collections has the same size, or that one dynamic integer of a pair is larger than the other, or that an integer is a valid subscript of a dynamic collection, or that a dynamic divisor is never zero.

I am interested in dependent types, and I believe they could augment Awelon in useful ways. But I also wish to introduce them in a way that:

* fits nicely with tacit programming
* doesn't require thinking about them except where desired
* works effectively internal to a confined subprogram or component 
* carries the proof through the type system, perhaps coupled to values
* does not introduce identity without explicit use of a uniqueness source

It may be that Awelon already has the tools it needs with sealer/unsealer pairs and a uniqueness source. Of course, if that's the case, developers would need to build a whole new metaprogramming layer within Awelon to take advantage of this. That might even be acceptable (just switch out a few imports, and maybe integrate with an IDE).

In any case, I need to learn a lot more about dependent type systems before taking this seriously.

### Dictionary/Wiki Mode

An alternative to importing individual modules might be to import a whole resource - e.g. a whole dictionary - where each of the 'this' module values in the dictionary becomes accessible by name, and substructure must always be accessed using the : (for protection against additions to the dictionary).

Not really sure about this though. Hmm. Maybe leave off the substructure? There wouldn't be any issue with 'ambiguity' the way I've defined it (based on expansion rather than source).

### Fractional and Negative Types (Potentially)

To complete the set of algebraic types `(a * b)` and `(a + b)`, developers gain access to fractional and negative types: `1/a` and `-a`. These concepts are formally described in ["The Two Dualities of Computation: Negative and Fractional Types" by Amr Sabry and Roshan P James](http://www.cs.indiana.edu/~sabry/papers/rational.pdf). 

Fractional types are incredibly useful for modeling:

* adaptive code: polymorphic based on downstream requirements or preferences
* [promises, futures](http://en.wikipedia.org/wiki/Futures_and_promises), useful for quick integration: make promises in one place (with receive), hold onto the fractional type for a while, then fulfill it in another.
* a sort of 'continuation passing' along a pipeline 
* static negotiations between upstream and downstream components

I don't have an intuition or use-case for negative types. But they're symmetric and elegant, so they'll be supported too, and hopefully will find some use someday. 

Awelon introduces just a few primitive behaviors, plus the associated types:

        receive     :: 1 -> (1/a * a)   
        return      :: (1/a * a) -> 1
        receive+    :: 0 -> (-a + a)
        return+     :: (-a + a) -> 0

Here 'receive' takes the `1/a` as input, and outputs `a`. And 'return' simply goes the other direction. These are basically identity functions, except that 'receive' obtains its input from the right. This requires some late-binding and laziness at compile-time, because we don't actually know the type or static values of `a` until it's returned. Awelon aims to be as dumb as possible about this: there is no cleverness, no implicit conversion from `1/(1/a)` to `a` that might make the direction of dataflow less clear. 

There are two major issues with fractional types.

First, developers can now express invalid data-cycles:

               <-- 1/a --<       THIS IS BAD
              /           \
        receive           return     
              \           /
               >--- a --->

Fortunately, these cycles are easy to detect statically. Unfortunately, these cycles are difficult to detect *compositionally*, i.e. in terms of shallow types without global constraints analysis. A consequence is that Awelon forbids negative and fractional types at the interface for dynamic behaviors, to keep the analysis simple. 

Second, the types and static value for 'a' is unknown. This could cause issues with compile-time decisions made on 'a' before the structure of 'a' is known. For example, if we access a named stack based on 'a', we might not know which named stack is accessed, and thus the state of all named stacks becomes unknown. Visualization could also be difficult. 

#### Promises Promises (Alternative)

If the second issue cannot be resolved to my satisfaction, I may need to find an alternative approach to futures, and receiving data from the right hand side. Fortunately, this isn't difficult! I can model futures as linear structures:

        newPromise :: 1 -> (wait * resolve)

In this case, wait and resolve are anonymous blocks, both with linear type. Developers applying the 'wait' block will obtain the promised value, while developers applying the 'resolve' block will provide it. The main difference is that now we have a clear instant in the Awelon program where the promise is resolved. Also, no symmetric support for sum types, and much less compositional or elegant.

That said, it isn't clear to me that this alternative improves anything. I could probably have fractional types 'wait' to process 'a' in any significant manner until I receive some knowledge about its type. Really, the issue might simply be that I need a notion of concurrent compilation as part of the visualization, and to rely on developers to not make major decisions with an unresolved type that might affect how the type is resolved.

I would prefer to have a more robust model, though... one that can resolve many cases. Perhaps I'll need to accept a degree of inference in the compile-time computation.



### Objects-Oriented Awelon

I can already model objects in Awelon - i.e. just create a tuple like:

        ("object" * (interface * privates))

The interface should be a linear-typed block, which takes a (privates * message) pair, and produces an (object' * response) pair. The block should respond to "copy" and "drop" messages (if only to provide a decent error message). The generic copy and drop operators can then be encoded to recognize objects. 

The private members could potentially be protected by a sealer/unsealer pair, but that seems a bit heavyweight for objects. (I don't really need a unique identity for each object.) I am considering whether I want to support sealing the object for this particular usage pattern, as a dedicated one-way sealer. 

        objectSeal :: (block * data) -> object{block,data}

Doing so might be more convenient for developers, and I could also have 'objectCompose' and 'objectFirst' patterns. My current thought, however, is that I should wait on this until I'm sure I've found a minimal but expressive approach. It isn't critical to have objects right away.


### Rewrite Rules, Equational Laws, or Macros?

I would like some way to specify that the sequence `swap swap` is equal to the empty program (or is equal but adds type constraints: `>=`), or more complicated proofs like `rot2 rot2` being equal to the empty program. I could still typecheck the original program. I'd also like some ability to prove equational properties, or validate them (maybe using some sort of quickcheck mechanism?).

The model for this could be easy. 

        rule "2swap" [swap swap] = []

But I doubt this would generalize well, e.g. if I want to say that sequentially functions two items is the same as composing the functions then mapping them once, I need some way to specify the functions? But maybe this could be implicit using something like:

        rule "2map" [[F1] map [F2] map] = [[F1] [F2] compose map] 

How hard would it be to generalize from this case, to one where the static functions are not found locally but distributed in the source code? I don't have good answers yet.

For now, optimizations will be left to a dedicated optimizer. 
