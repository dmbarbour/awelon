
# Awelon

Awelon is a richly typed, compiled language for reactive demand programming (RDP). RDP is a general purpose, reactive dataflow model that addresses challenges involving open, distributed, heterogeneous systems (see AboutRDP for more). Awelon is intended to be a usable programming language for humans, a feasible distribution language for RDP code and software components, and a viable target for RDP development environments (some of which might be more visual in nature).

In Awelon, an application or component can describe code that executes integrates with distributed services, or executes in multiple OS level processes.

This Haskell project will provide an initial compiler for Awelon.

## Tacit Programming

Awelon code is [tacit](http://en.wikipedia.org/wiki/Tacit_programming). Juxtaposition is composition. All inputs are implicit, in an environment. However, unlike most tacit languages, Awelon's environment exists only as a compile-time type, and all manipulations to it are pure. Developers use the environment at a *conceptual* level - a way to organize resources and workflows, and to integrate them. 

At runtime, the environment is gone and the data plumbing is compiled completely away.

### Stack Based Programming

Like many tacit programming languages, Awelon supports stack-based programming. One of the key aspects of tacit programming is how *literals* are handled - e.g. if a developer writes `10 11 12`, what happens? Somehow, those literals must be added to the environment. For example, we can push literals onto a stack.

        42 %  e ~> (Number * e)
        12.3 %  e ~> (Number * e), exact 123/10
        "literal string" %  e ~> (Text * e)
        [swap] %  e ~> (Block * e)

In Awelon, literals are simply numbers (rationals or integers), text, or blocks of code. Developers often choose to distinguish rational numbers from integrals (e.g. by testing that the number is integral), but Awelon lumps them together. Also, due to ad-hoc polymorphism, Awelon essentially treats static blocks as untyped until they're applied. 

Literals have 'Static' types, meaning their values are available at compile-time both for operations and decisions. Static types also are the key ingredient to make tacit programming work with an arrowized model, enabling us to model 'first' without use of explicit parameters.

        %  first :: (x ~> x') -> ((x*y) ~> (x'*y))     HASKELL
        %  first :: ((x ~> x') * (x * y)) ~> (x' * y)  AWELON (candidate prim)
        [swap] first %  ((a*b)*c) ~> ((b*a)*c)

Support for partial application with 'first' is valuable for both code reuse and expressiveness. Awelon's model for 'first' has greater expressiveness because it allows the specification of the behavior to be cleanly separated from its call site, thus avoiding the need for an orthogonal abstraction model (such as lambdas). There is a price: Awelon's first is expressive enough to cause trouble. The traditional non-terminating lambda (`(\x->(x x) \x->(x x)`) can be represented (`[copy apply] [copy apply] first`).

To help illustrate what tacit code often looks like, I'll define a few more functions that assume a single stack:

        % Assuming available behaviors:
        %  assocl :: (x * (y * z)) ~> ((x * y) * z)         
        %  swap   :: (x * y) ~> (y * x)                     
        %  intro1 :: x ~> (Unit * x)                        
        %  elim1  :: (Unit * x) ~> x                        
        %  copy   :: x ~> (x * x)                           
        
        apply  = [intro1 swap] second first swap elim1      %  (Static (x ~> x') * x) ~> x'
        second = swap [swap] first swap first swap          %  (Static (y~>y') * (x*y)) ~> (x*y')
        assocr = swap assocl swap assocl swap               %  ((x * y) * z) ~> (x * (y * z))
        roll2  = assocl [swap] first assocr                 %  (x * (y * z)) ~> (y * (x * z))
        roll3  = [roll2] second roll2                       %  (a * (b * (c * d)))) ~> (c * (a * (b * d)))
        roll4  = [roll3] second roll2
        dup    = [copy] first assocr                        %  (a * e) ~> (a * (a * e))
        pick1  = [dup] second roll2                         %  (a * (b * e)) ~> (b * (a * (b * e)))
        pick2  = [pick1] second roll2
        %  Note: the above definitions are for illustrations; those
        %  in use may be different (e.

Tacit code is easy to incrementally parse and process, and similarly easy to generate. Those are useful properties for Awelon's goals as a distribution or target language. Tacit code is naturally composable, and should be effective for representing software components. Avoiding direct manipulation by names enables many of RDP's invariants to be enforced constructively. Tacit code has very little syntactic noise. 

However, tacit code has a weakness: much of the data plumbing becomes explicit. Data plumbing words become a form of semantic noise, unrelated to the problem domain. For productive development, it is necessary that these data plumbing operations become simple, intuitive, fading into the background of the developer's attentions. 

*ASIDE:* Traditional languages also have difficulty with data plumbing - it just happens in higher layers: concurrency, callbacks, collections. RDP is designed to address many challenges in those higher layers.

### Awelon's Extended Environment

A single stack environment can feel cramped. This is especially the case for a model like RDP, where we often have multiple tasks or concurrent workflows that require ad-hoc integration of intermediate results. Awelon favors *multiple stacks for multiple tasks*, and thus provides an extended environment: 

        (currentStack * extendedEnv)

Stack programming operators like `roll` or `pick` are tweaked to operate on the current stack. Literals are now added to the current stack, ensuring that the extendedEnv remains easily accessible without needing to dig it up after adding each literal:

        42                  %  (s*e) ~> ((Number * s) * e) 
        12.3                %  (s*e) ~> ((Number * s) * e) exact 123/10
        "literal string"    %  (s*e) ~> ((Text   * s) * e)
        [swap]              %  (s*e) ~> ((Block  * s) * e)

Other than that constraint, developers could create any extended environment they want. Adjusting the Awelon environment could be very useful to simplify integration with visual IDEs (i.e. such that gestures in the IDE correspond directly to streams of words). However, Awelon does provide a standard extended environment that should be expressive enough for human users:

        (currentStack * (hand * (currentStackName * listOfNamedStacks)))

The "hand" is another stack. Developers use `take` and `put` to move items between the hand and the current stack, and `juggle` to rotate items in the hand. The main purpose of the hand is to carry items between stacks, i.e. an intuitive approach to anonymous data plumbing. However, the hand is convenient even for operating on a single stack: one can pick up a few objects, do some deep-stack operations, then drop the objects again. This seems easier than carefully rolling objects to the top of the stack then back down.

Finally, there is a list of named stacks. Each element in the list is a (name*stack) pair, where the name is simply a text literal. Named stacks have a variety of purposes:

* Programmers can easily navigate stacks by name. This is useful for stacks that represent different tasks. The hand is convenient for carrying items between stacks. In a sense, this is a "goto", but it's more about the programmer's workflow than the program's.
* Programmers can `load` and `store`, obtaining values from a remote stack. This is useful for stacks that represent variables or registers, which is often convenient for long-term storage. 
* Named stacks can also support ad-hoc environment extensions, e.g. we could model aspect-oriented programming in Awelon in terms of explicit join-points (`"foo" signal`) and advice in the form of blocks. 

Named stacks require compile-time metaprogramming to systematically compare names in a list. Awelon's support for compile-time introspection and metaprogramming is discussed in a later section on polymorphic behaviors.

### Conditionals and Choices

In an arrowized model like RDP, choice is expressed by computing a sum type `(x + y)`, then operating on it. This has an advantage of being extensible: developers can keep the choice open for as long as they wish. It also works well in a distributed system: the x and y values may represent different routes or paths in a network. Choice has the basic data plumbing primitives similar to products - left, mirror, assocl+, intro0, etc.. I won't focus on those here. Rather, it's the interaction between sums and products that become interesting. 

Awelon actually has two sum types: `(x + y)` for dynamic choices, and `(x | y)` for static choices. Static choice is useful for metaprogramming. Static choices can do anything dynamic choices can do. They even support the same words, so anything written for a `(x + y)` will work for `(x | y)`. However, static types allow a few powerful and convenient operations that take advantage of compile-time knowledge.

Choices can always be merged, i.e. losing information about which branch we were on. 

        % conjoin :: ((x*y)+(x*z))~> (x*(y+z))  
        % forget1 :: (1+1)~>1                   
        % merge :: (x + x) ~> x  
        merge = [intro1 swap] left mirror [intro1 swap] left 
                conjoin swap [forget1] first elim1

In many cases, this merge is implemented by combining the in-memory signal representations. However, a logical merge is also possible where both branches simply are extended with the same subprograms from that point forward. An optimizer may deem it more optimal to extend for a little while before merging, especially if a lot of signals are dropped. 

Note that dynamic merge requires input types be exactly the same. This is one place where it's important to understand that static values (text, numbers) are formally considered distinct types, and must match exactly. Static choice offers a more powerful form of 'merge':

        % forget :: (x|y)~>x OR (x|y)~>y

Forget takes advantage that programmers and compilers are actually aware of the type. It becomes the basis of ad-hoc polymorphism, where the output type for a behavior can depend (in an introspective, non-parameteric way) on the input type. Awelon doesn't have Haskell-like support for typeclasses, nor does it have true dependent types; 'forget' represents a middle ground between the two.

Also, static numbers and text can be extracted from a choice. This can be useful if there is ever a need to compare choices. 

        % extractNumber :: (N*x)+y ~> N*(x+y)
        % extractText   :: (T*x)+y ~> T*(x+y)

At the moment, blocks cannot be extracted.

Going the other direction, developers can push certain data from the environment into a choice. For static choice, this is trivial: ANY data can be distributed into a static choice. Similarly, static data can be distributed into any static choice:

        % disjoinStatic  :: (x * (y | z)) ~> ((x * y) | (x * z))
        % disjoinStatic+ :: (x + (y | z)) ~> ((x + y) | (x + z))
        % disjoinNumber  :: (N * (y + z)) ~> ((N * y) + (N * z))
        % disjoinText    :: (T * (y + z)) ~> ((T * y) + (T * z))
        % disjoinBlock   :: (B * (y + z)) ~> ((B * y) + (B * z))

The real challenge, however, is injecting runtime signals into a dynamic choice. This isn't easy in Awelon due to the potentially distributed nature of runtime signals. Developers must carefully arrange dynamic elements into the right partitions to provide enough information for the disjoin:

        % disjoinSignal  :: (x@p * ((Unit@p * y) + z)) ~> ((x@p * y) + (x@p * z))

Here `x@p` means a runtime signal of value type x in partition p, where p also contains the latency info. So the above type indicates we must get a "masking" signal `Unit@p` into the same latency and partition as the signal we're splitting. To make this happen may require partition crossings or delays by one signal or the other. Once the two signals are in the same partition, the mask is applied: everything in 'x' matched by the mask goes into the left branch, everything not matched goes into the right branch. 


## Awelon's Module System

Awelon has a very simple module system. Essentially, a module consists of one import line followed by one or more definition lines. An import line contains a comma separated list of module names. The association between a module name and a module definition is externally determined (e.g. by file name). Module names must be simple words, i.e. valid as both Awelon export words and filesystem/URL components. 

        import common, packageWithLongName AS p, foo
        this = [f] _s
        f = foo [p:bar foo:bar] ap %  here 'foo' means 'foo:this'
        _s = assocl swap rot2 first swap
        % TODO: replace this with a usable module...

All words defined in a module are exported from it, except those prefixed with the underscore character such as _s above. (Imported words are not re-exported.) All words are also available prefixed with 'module:' from which they come. Words may be defined in any order within a module. Imports may be cyclic. However, *definitions* may not be cyclic or recursive, nor may they even appear to be. If developers wish to re-export a word, they must use the prefixed form on the right hand side, e.g. `bar = p:bar`. Also, if there is any ambiguity in a word's definition (i.e. after expansion to primitives), the prefixed form must be used.

The word 'this' has special meaning. Within a module, 'this:' may be used as the prefix to disambiguate a module's words. When imported, the word 'this' is mapped instead to the import name, e.g. 'foo' will mean 'foo:this'. Awelon is intended for component based software; I believe Awelon's use of 'this' has nice connotations to encourage treating modules as software components.

Awelon's syntax is very simple. Imports and new definition must start at the beginning of a new line. If a definition extends for multiple lines, there must be spacing near the front. Code itself is just a space-separated sequence of words and literals. Awelon has simple line comments starting with `%`. 

There are no default words or imports in Awelon.

Only literals can be used without an import. If there is something like a common prelude or standard environment, it must be on the import line. Consequently, it is not difficult for developers to create alternative programming environments (the only requirement is that literals are added to the first element of a pair).

*NOTE:* I believe developers shouldn't need more than a line of boiler plate before they get to useful work, which is why I allow multiple imports on one line. I discourage hierarchical module systems. Libraries should generally be exported at the package layer. Ambiguity is rare in practice, and resolved easily enough. Modules intended to serve as software components are encouraged to export 'this' and little else (perhaps authoring info, etc.).

### Automatic Testing

Static tests are possible by convention. Developers define words starting with 'test' or '_test', and an Awelon compiler or IDE will process these in a simple environment - containing a static uniqueness source and forget permission and the like, but not much else. These tests will pass unless they explicitly error out or overrun their quota. A passing test may explicitly emit warnings. In some cases, a test might be useful to help visualize the static behavior for a word in different contexts.

Continuous runtime testing is also an interesting possibility. Potentially an IDE might support runtime tests as well, perhaps requiring an indicator in the name. But a runtime test is essentially an application, and it might be better to treat it as such.

*NOTE:* Test words are normal words, just interpreted and processed by convention.

### Awelon Virtual Machine

The Awelon Virtual Machine (AVM) is a description of an RDP-based programming environment. 

This description includes types, partitions, and behavior primitives. It is a complete description, repeating even the ‘standard’ primitives like first and compose. Unlike a traditional VM, an AVM can describe heterogeneous systems, where different partitions have different behavior primitives and asymmetric communication. An AVM may additionally include: equational laws, proposed rewrite rules, and other properties. An application model would describe behavior types for applications or pluggable applets, along with some English text about how it is used. 

Within the avm, the type of a behavior is represented as a block to execute on a type descriptor. The 

The AVM is represented as an Awelon module that operates in a restricted subset of Awelon to construct its description (i.e. as opposed to using JSON or XML). This restricted subset is provided by a module called 'avmboot'. The AVM may import avmboot or other modules that ultimately depend on avmboot (rather than avmprim). The parser must understand avmboot implicitly during the bootstrap process. It provides just a few primitives for static operations.

Essentially, the AVM is a machine-processed standards document. There won't be many AVMs (probably the main line, a few experimental branches, maybe a didactic subset). After Awelon matures, the AVM will evolve very slowly. 

An Awelon application is limited to one AVM. Often, which AVM will be implicit in the development environment, and a compatible AVM can be switched in transparently. (An incompatible AVM can also be switched in; it just breaks your code.) In general, compilers will be limited to a single evolving line of AVMs.

### Primitive Words

A special module called 'avmprim' is created automatically from the AVM. This contains all the primitive words defined in the AVM, and is the ultimate root of all modules in an Awelon project. The module 'avmboot' contains a subset of primitive words, which may be used by the avm (or any module the avm uses).

In general, developers do not directly import the primitives module. Primitives are too far removed from the more complicated multi-stack environments presented to developers. Even if they want low-level primitives, developers should use a module that extends them with all the common helpers (which I might name 'pure').

### Awelon Objects

Modules in Awelon are called Awelon Objects or AOs. The extension **.ao** should be used for modules represented in the filesystem (though the plan is for Awelon to be expressed in a more wiki-like environment). AO files should be encoded in UTF-8. The name of the module is simply the filename excluding the extension. If there are ambiguous filenames (e.g. with installed packages), they must be resolved externally.

## Metaprogramming via Ad-hoc Polymorphism

Awelon supports ad-hoc polymorphism by introspection of types and static values. For example, developers can examine a type to see whether it is a product.

        % testIsProduct :: x -> (x=(a*b)|x)  ... conceptually

Introspections can also test numbers, compare texts, and result in static choices. Developers can take different actions on different branches, may assert certain properties, then may 'forget' which branch they were on - even if the choices result in different types.

A good example where this is useful is to create a generic copy or drop operator. Awelon does not support copy or drop generally because affine types cannot be copied, and relevant types cannot be dropped, and sealed types cannot even be introspected (beyond determining their owner). 

As noted earlier, Awelon is capable of modeling a fixpoint combinator, and so can represent these decisions recursively - e.g. to build a copy or drop operation, or to look up a stack by name, in a list. 

Combined with fixpoint combinators, developers can write recursive compile-time functions - e.g. to build a recursive copy or drop operation that works unless there is a linear type or sealed value preventing it. 


### Recursion and Repetition

A common situation, as with 'copy', is that developers need to operate on deep structures. For basic tree structures, we can take advantage that the type of an Awelon program is always finite. A simple recursion combinator for a tree might look like:

        mapTree :: OnLeaf * JoinTrees * Trees ~> whatever

Of course, we may want more complicated combinators: static folds, searches, etc.. Awelon doesn't build in any particular combinators, and instead leverages its compile time expressiveness. Awelon is Turing complete:

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


## Awelon's Types

Awelon is very richly typed, though not in a conventional manner. Awelon types are computed - developers don't even have the option of declaring types. If developers wish to enforce specific types, they must use introspective compile-time behaviors. For static numbers, text, and blocks, the values are formally considered to be part of the type (which is useful to know for collections).

Awelon provides basic products, sums, statics, and collections. Awelon must also support a variety of runtime value types. And then there are a few more 'rich' types that make a huge difference in the programming experience and API design:

* fractional and negative types (right-to-left dataflow)
* linear types (enforcing contracts, protocol completion)
* uniquness source ('create' unique values or exclusive state)
* sealer/unsealer pairs (modeling encryption, ADTs)
* dynamic collections (sets, vectors)
* dynamic behaviors (runtime metaprogramming, linking)
* expiration and ripening (control latency)

### Fractional and Negative Types

To complete the set of algebraic types `(a * b)` and `(a + b)`, developers gain access to fractional and negative types: `1/a` and `-a`. These concepts are formally described in ["The Two Dualities of Computation: Negative and Fractional Types" by Amr Sabry and Roshan P James](http://www.cs.indiana.edu/~sabry/papers/rational.pdf). 

Fractional types are incredibly useful for modeling:

* adaptive code: polymorphic based on downstream requirements or preferences
* [promises, futures](http://en.wikipedia.org/wiki/Futures_and_promises), useful for quick integration: make promises in one place (with receive), hold onto the fractional type for a while, then fulfill it in another.
* a sort of 'continuation passing' along a pipeline 
* static negotiations between upstream and downstream components

I don't have an intuition or use-case for negative types. But they're symmetric and elegant, so they'll be supported too, and hopefully will find some use someday. 

Awelon introduces just a few primitive behaviors, plus the associated types:

        receive     :: 1 ~> (1/a * a)   
        return      :: (1/a * a) ~> 1
        receive+    :: 0 ~> (-a + a)
        return+     :: (-a + a) ~> 0

Here 'receive' takes the `1/a` as input, and outputs `a`. And 'return' simply goes the other direction. These are basically identity functions, except that 'receive' obtains its input from the right. This requires some late-binding and laziness at compile-time, because we don't actually know the type or static values of `a` until it's returned. Awelon aims to be as dumb as possible about this: there is no cleverness, no implicit conversion from `1/(1/a)` to `a` that might make the direction of dataflow less clear. 

A potential issue is that developers can now express invalid data-cycles:

               <-- 1/a --<       THIS IS BAD
              /           \
        receive           return     
              \           /
               >--- a --->

Fortunately, these cycles are easy to detect statically. Unfortunately, these cycles are difficult to detect *compositionally*, i.e. in terms of shallow types without global constraints analysis. A consequence is that Awelon forbids negative and fractional types at the interface for dynamic behaviors, to keep the analysis simple. 

Use of beat and backbeat with block composition can also be used to twist inputs to different sides of blocks. The most useful operation is probably just to flip a block around and install it backwards. e.g. `(x~>y)~>(1/y~>1/x)`.

### Linear Types

Linear types are a very simple concept: values that cannot simply be dropped or copied, but must properly be consumed. These types are useful for enforcing that responsibilities are discharged, that protocols and handshakes are completed, that resources are used carefully, and so on.

In Awelon, linear types are simply modeled by NOT providing drop (`x ~> 1`) or copy (`x ~> x*x`) primitives for that type. (Awelon has an easier time of this than most languages - no worries about variables, closures, intermediate state, and so on.) Awelon can also model affine types (no copy) or relevant types (no drop). Many use-cases for linear types in Awelon involve static values. 

One simple linearity primitive is to mark blocks as linear:

        linearize :: (x~>y) ~> LinearBlock(x~>y)  

Linear blocks are consumed normally by first, left, etc.. They can also be composed, resulting in a larger linear block. But they cannot be dropped or copied. Basically, it's almost an invisible type tag. (*Note:* linearize actually involves marking a block both affine and relevant.)

In addition to enforcing properties, substructural types can help programmers organize their tasks - i.e. the type-system supports those "get back to this!" flags and "to do" lists.

### Uniqueness Source

The uniquness source is a static object that represents a source of "unique values". Developers can use this object for to create sealer/unsealer pairs or exclusive state resources. Due to the idempotence of RDP, Awelon primitive behaviors cannot 'create' a uniqueness source: if an application has uniqueness at all, the source must be an argument to the application. Most application models will provide uniqueness, though.

The uniqueness source can be dropped, but it cannot be copied in the traditional sense. Rather than copies, the source can be "forked", but each fork requires programmers provide a text identifier:

        fork :: (Text * U!) ~> (U! * U!')

The goal of this design is to ensure unique values are *stable* across changes in source code. The text provides a stable directory-like structure: we can think of 'fork' as a 'mkdir' operation in a filesystem, with the first output being the fresh directory and the second output being the modified parent directory that typefully forbids a second 'fork' with the same child name. Typically, the child (which has a fresh namespace) will be passed to a subprogram or otherwise consumed, while the parent will be stored away in the hand for later use. 

This resulting stability is valuable for live programming, debugging, runtime update, and orthogonal persistence. The text - forming a verifiably unique path in the code - is also usable in the implementation of unique values.


### Sealer/Unsealer Pairs

A sealer/unsealer pair can be created if you have a uniqueness source:

        newSealerUnsealerPair :: (Text * U!) ~> ((Sealer U! * Unsealer U!) * U!')
        type Sealer u = x ~> Sealed u x
        type Unsealer u = Sealed u x ~> x

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

        rot3 :: (a*(b*(c*d))) ~> (c*(a*(b*d)))

Between intro1, elim1, assocl, swap, and rot3, any parametric data plumbing can be expressed block-free. This enables 'first' to be used on targets in the stack or hand... albeit after sufficient environment manipulations. A few examples of block-free definitions:

        rot2 = intro1 rot3 intro1 rot3 elim1 elim1  % (a * (b * c)) ~> (b * (a * c))
        rot4 = assocl rot3 rot2 assocr rot3         % (a * (b * (c * (d * e)))) ~> (d * (a * (b * (c * e))))
        rot5 = assocl rot4 rot2 assocr rot3         % etc...
        zip2 = assocr rot3 rot2 assocl              % ((a * b) * (c * d)) ~> ((a * c) * (b * d))
        take = zip2 rot2                            % ((x * s) * (h * e)) ~> (s * ((x * h) * e))
        put  = rot2 zip2                            % (s * ((x * h) * ee)) ~> ((x * s) * (h * ee))
        reifyStack =  intro1 rot2 assocl            % (s * e) ~> ((s * unit) * e) - stack becomes object on stack

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

        newAgentResource :: (U! * Text) * (B * N) ~> (U!' * A)

To construct an agent resource takes four arguments:

* a static uniqueness source and a Text value indicating the unique child
* a behavior that will receive its unique source and a `Unit@p` signal.
* a number: how long the agent will remain active after demands are gone

The uniqueness source and text gives every agent resource a stable name and stable subdirectory for its own state resources. The agent behavior cannot have any substructural type (not affine, not relevance); logically, the agent is external (super-structural). The extra delay after demands are eliminated can help 'smooth over' cases where the agent would otherwise be bouncing too quickly between active and inactive; it can also provide a grace period for explicit cleanup. But it probably shouldn't be longer than a few seconds. 

The result is the parent unique source and the agent, which is simply a block. 


### Debugging Primitives

Awelon developers can emit a compile-time error or warning if they need one.

        error :: (Text*x) ~> x
        warn  :: (Text*x) ~> x

A warning or error will apply if 'x' is ultimately on a live branch of a static computation. The given text can be computed statically; in addition, source location and type info will be provided. 

#### Signal Sinks

A trivial but useful form of single-writer state is to have a 'stateful signal' that is continuously updated by inputs to it, but that holds its old value (along with the time of last synchronization) after disruption (perhaps with an expiration). This concept is used to send Awelon applications for remote, disruption-tolerant execution.

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



### No Dependent Types? (Wishlist)

Awelon's static phase essentially supports values in the type-system and supports metaprogramming. But Awelon does not have any support for dependent types at runtime, e.g. I cannot assert that a pair of dynamic collections has the same size, or that one dynamic integer of a pair is larger than the other, or that an integer is a valid subscript of a dynamic collection, or that a dynamic divisor is never zero.

I am interested in dependent types, and I believe they could augment Awelon in useful ways. But I also wish to introduce them in a way that:

* fits nicely with tacit programming
* doesn't require thinking about them except where desired
* works effectively internal to a confined subprogram or component 
* carries the proof through the type system, perhaps coupled to values
* does not introduce identity without explicit use of a uniqueness source

It may be that Awelon already has the tools it needs with sealer/unsealer pairs and a uniqueness source. Of course, if that's the case, developers would need to build a whole new metaprogramming layer within Awelon to take advantage of this. That might even be acceptable (just switch out a few imports, and maybe integrate with an IDE).





