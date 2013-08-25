
# Awelon

Awelon is a richly typed, compiled language for reactive demand programming (RDP). RDP is a general purpose, reactive dataflow model that addresses challenges involving open, distributed, heterogeneous systems (see AboutRDP for more). Awelon is intended to be a usable programming language for humans, a feasible distribution language for RDP code and software components, and a viable target for RDP development environments (some of which might be more visual in nature).

In Awelon, a single application can include code that executes across a distributed network. 

This Haskell project will provide an initial compiler for Awelon.

## Tacit Programming

Awelon code is [tacit](http://en.wikipedia.org/wiki/Tacit_programming). Juxtaposition is composition. All inputs are implicit, in an environment. However, unlike most tacit languages, Awelon's environment exists only as a compile-time type, and all manipulations to it are pure. Developers use the environment at a *conceptual* level - a way to organize resources and workflows, and to integrate them. 

At runtime, the environment is gone and the data plumbing is compiled completely away.

### Stack Based Programming

Like many tacit programming languages, Awelon supports stack-based programming. One of the key aspects of tacit programming is how *literals* are handled - e.g. if a developer writes `10 11 12`, what happens? Somehow, those literals must be added to the environment. For example, by pushing it onto a stack.

        42 %  e ~> (Static Integer * e)
        12.3 %  e ~> (Static Rational * e), exact 123/10
        "literal string" %  e ~> (Static Text * e)
        [swap] %  e ~> (Static ((a * b) ~> (b * a)) * e)

In Awelon, literals are simply numbers, text, or blocks of code. Developers often choose to distinguish rational numbers from integrals (e.g. by testing that the number is integral), but Awelon lumps them together. Also, due to polymorphism, Awelon essentially treats blocks as untyped until they're applied. Literals have 'Static' types, meaning their values are available at compile-time both for operations and decisions. Static types also are the key ingredient to make tacit programming work with an arrowized model, enabling us to model 'first' without use of explicit parameters.

        %  first :: (x ~> x') -> ((x*y) ~> (x'*y))              HASKELL
        %  first :: (Static (x ~> x') * (x * y)) ~> (x' * y)    AWELON
        [swap] first %  ((a*b)*c) ~> ((b*a)*c)

Support for partial application with 'first' is valuable for both code reuse and expressiveness. Awelon's model for 'first' has greater expressiveness because it allows the specification of the behavior to be cleanly separated from its call site, thus avoiding the need for an orthogonal abstraction model (such as lambdas). There is a price: Awelon's first is expressive enough to cause trouble. The traditional non-terminating lambda (`(\x->(x x) \x->(x x)`) can be represented (`[copy apply] [copy apply] first`).

To help illustrate what tacit code often looks like, I'll define a few more functions:

        %  assocl :: (x * (y * z)) ~> ((x * y) * z)         PRIMITIVE
        %  swap   :: (x * y) ~> (y * x)                     PRIMITIVE
        %  intro1 :: x ~> (Unit * x)                        PRIMITIVE
        %  elim1  :: (Unit * x) ~> x                        PRIMITIVE
        %  copy   :: x ~> (x * x)                           polymorphic; see below
        
        apply  = [intro1 swap] second first swap elim1      %  (Static (x ~> x') * x) ~> x'
        second = swap [swap] first swap first swap          %  (Static (y~>y') * (x*y)) ~> (x*y')
        assocr = [swap] first swap assocl [swap] first swap %  ((x * y) * z) ~> (x * (y * z))
        roll2  = assocl [swap] first assocr                 %  (x * (y * z)) ~> (y * (x * z))
        roll3  = [roll2] second roll2                       %  (a * (b * (c * d)))) ~> (c * (a * (b * d)))
        roll4  = [roll3] second roll2
        unroll2 = roll2
        unroll3 = unroll2 [unroll2] second                  %  (c * (a * (b * d))) ~> (a * (b * (c * d)))
        unroll4 = unroll2 [unroll3] second
        dup    = [copy] first assocr                        %  (a * e) ~> (a * (a * e))
        pick1  = [dup] second roll2                         %  (a * (b * e)) ~> (b * (a * (b * e)))
        pick2  = [pick1] second roll2
        %  Note: the above definitions are for illustrations; those
        %  in use may be different (e.

Tacit code is easy to incrementally parse and process, and similarly easy to generate. Those are useful properties for Awelon's goals as a distribution or target language. Tacit code is naturally composable, and should be effective for representing software components. Avoiding direct manipulation by names enables many of RDP's invariants to be enforced constructively. Tacit code has very little syntactic noise. 

However, tacit code has a weakness: much of the data plumbing becomes explicit. Data plumbing words become a form of semantic noise, unrelated to the problem domain. For productive development, it is necessary that these data plumbing operations become simple, intuitive, fading into the background of the developer's attentions. 

*ASIDE:* Traditional languages also have difficulty with data plumbing - it just happens in higher layers: concurrency, callbacks, collections. RDP is designed to address many challenges in those higher layers.

Data plumbing on a stack is relatively simple: developers mostly operate near the top of the stack, and occasionally roll or copy elements to the top. It does not take long to become familiar with these manipulations, and stacks are effective for linear operations. Unfortunately, a single stack is inadequate for representing incremental tasks or concurrent workflows, especially if there is some interweaving of intermediate results. Also, there are cases where devilish data plumbing problems on a single stack would be solved trivially with just a little extra scratch space.

#### Multi Stack Environment

Awelon's static environment contains a non-empty *list* of stacks. There is always a current stack, and all the single stack operators now apply to the current stack. Literals are added to the current stack. Developers can add new stacks, drop old stacks, and navigate between them. These additional stacks quickly find use by developers: different stacks for different tasks, scratch spaces, temporary storage, and that stuff you never get back to but keep thinking you might. 

        %  List of Stacks:  (stacksLeft * (currentStack * stacksRight))
        %  stepLeft  :: ((sT * sL) * (sC * sR)) ~> (sL * (sT * (sC * sR)))
        %  stepRight :: (sL * (sC * (sT * sR))) ~> ((sC * sL) * (sT * sR))
        stepLeft = [swap] first assocr
        stepRight = assocl [swap] first [assertIsProduct] second

        %  New, dup, and drop for stacks create or destroy a stack to your right
        %  
        %  newStack  :: (sL * (sC * sR)) ~> (sL * (sC * (Unit * sR)))
        %  dupStack  :: (sL * (sC * sR)) ~> (sL * (sC * (sC * sR))) %  if non-linear sC
        %  dropStack :: (sL * (sC * (sD * sR))) ~> (sL * (sC * sR)) %  if non-linear sD  
        newStack = assocl [swap] first [intro1] second
        copyStack = [[copy] first assocr] second
        dropStack =  [[dropFirst] second] second
       
        %  more basics
        erase   :: x ~> Unit        %  for non-linear x
        dropFirst = [erase] first elim1  %  (e * x) ~> x, for non-linear e
        dropSecond = swap dropFirst      %  (x * e) ~> x, for non-linear e

Of course, multiple stacks are useless if we don't have an effective way to move elements between them. I could potentially model throwing things between stacks, but there is a better way.

*NOTE:* Many of the above definitions will don't account for blocks being added to the current stack in the middle of the manipulation. Awelon uses a 'block-free' encoding for the basic environment manipulations. See `stdenv.ao` for details.

### Hands to Carry Things

Awelon's multi stack environment is further extended with the concept of *hands*. With one hand, a developer can take and put, moving objects easily and intuitively from one stack to another, or even moving stacks. Hands would be convenient even for operations on a single stack, i.e. rather than rolling stuff to the top, just pick up the top several elements and do some work, then put several elements back down. 

Hands offer a simple, intuitive, and expressive approach to data plumbing. 

Hands are modeled as a pair of stacks, and are paired with the current environment, such that our overall environment is a structure of the form: `(sL*(sC*sR))*(hL*(x*hR))`. Here current stack, and x is the active object - if any. Unlike the list of stacks, the developer's hands may be empty.

        %  take :: (sL*((x*sC)*sR))*(hL*hR) ~> (sL*(sC*sR))*(hL*(x*hR))
        take = [[assocr] second roll2] first assocr roll2 [roll2] second

        %  swapStackHand :: (sL*(sC*sR))*(hL*hR) ~> (sL*(hR*sR))*(hL*sC)
        %  left as exercise for reader :)

By convention, the right hand is used for volatile operations (take, put, etc.). The left hand is used instead for environment extensions based on introspection and metaprogramming - such as modeling named stacks. 

*NOTE:* The programmer's hands are generally NOT passed to partially applied behaviors. The intuition here is that the developer is installing a behavior that will then execute without further guidance or interference. Also, conversely, the installed behavior must not affect the contents of the programmer's hand. Arguments to a partially applied behavior must be explicitly provided. 

### Named Stacks via Metaprogramming

Awelon will support storing and loading elements by name. Use of names is a valid and convenient solution for many data plumbing problems: simply tuck a value away with an arbitrarily chosen name, then later recover it using the same name. Names are easier to remember than relative locations. 

Names are modeled by use of polymorphic, introspective behaviors (see later section) that will search the left hand for a named stack, then add or remove an item (adding or removing the stack as appropriate). The use of stacks can serve a similar role to lexical scope, though developers must be careful to erase names when they exit logical scope. 

        "foo" store    %  store element to "foo" stack (removes from current stack)
        "foo" load     %  load element from "foo" stack (removes from foo stack)
        "foo" loadCopy %  load, dup, store 
        "foo" erase    %  load, drop
        %  possibly loadStack and storeStack operations, too.

The default movement semantics are suitable for linear types, or cases where developers wish to model updates. (These variables are 'pure' from Awelon's perspective.) Unlike traditional programming, developers must explicitly erase words th

Names can become syntactic noise and clutter if overused. I prefer to use names sparingly, for long-lived elements that spend most of their time in the background. Other programmers may prefer more extensive use.

## Awelon's Module System

Awelon has a very simple module system. Essentially, a module consists of one import line followed by one or more definition lines. An import line contains a comma separated list of module names. The association between a module name and a module definition is externally determined (e.g. by file name). Module names should be simple alphanumeric words. Modules may be associated with local names. Imports may be cyclic.

        import common, packageWithLongName AS p, foo
        this = [f] _s
        f = foo [p:bar foo:bar] first %  here 'foo' means 'foo:this'
        _s = swap [swap] first swap first swap

Words may be defined in any order within a module. However, definitions cannot be cyclic or recursive.

All words defined in a module are exported from it, except those prefixed with the underscore character such as _s above. (Imported words are not re-exported.) All words are also available prefixed with 'module:' from which they come. If there is any potential ambiguity for a word (i.e. if it is exported from two of the modules, including this one), it becomes necessary to use the prefixed form.

The word 'this' has special meaning. Within a module, 'this:' may be used as the prefix for a module's words. When imported, the word 'this' is mapped instead to the import name, e.g. 'foo' means 'foo:this' after importing foo (assuming no ambiguity). Also, the word 'this' in Awelon serves the same role as 'main' in many other languages. Awelon is intended for component based software; I believe 'this' has nice connotations for treating modules as possible software components.

All imports and definitions must start at the beginning of a new line of text, and continue to the end of the logical line. A logical line continues until a non whitespace character begins a new line of text, i.e. so developers can distribute some code or imports vertically. Awelon has simple line comments starting eith `%`. 

        import common
        % take :: (sL*((x*sC)*sR))*(hL*hR) ~> (sL*(sC*sR))*(hL*(x*hR))
        take = [[assocr] second roll2] first %  (x*(sL*(sC*sR)))*(hL*hR)
               assocr roll2 [roll2] second

Note: there are no default words or imports in Awelon. Only literals can be used without an import.

*NOTE:* I believe developers shouldn't need more than a line of boiler plate before they get to useful work, which is why I allow multiple imports on one line. I discourage deep, hierarchical module systems. Libraries should generally be exported at the package layer. Ambiguity is rare in practice, and resolved easily enough. Modules as software components are encouraged to export 'this' and little else.

### Awelon Virtual Machine

The Awelon Virtual Machine (AVM) is a description of an RDP-based programming environment. 

This description includes types, partitions, and behavior primitives. It is a complete description, repeating even the ‘standard’ primitives like first and compose. Unlike a traditional VM, an AVM can describe heterogeneous systems, where different partitions have different behavior primitives and asymmetric communication. An AVM may additionally include: equational laws, proposed rewrite rules, and other properties. An application model would describe behavior types for applications or pluggable applets, along with some English text about how it is used. 

Within the avm, the type of a behavior is represented as a block to execute on a type descriptor. The 

The AVM is represented as an Awelon module that operates in a restricted subset of Awelon to construct its description (i.e. as opposed to using JSON or XML). This restricted subset is provided by a module called 'avmboot'. The AVM may import avmboot or other modules that ultimately depend on avmboot (rather than avmprim). The parser must understand avmboot implicitly during the bootstrap process. It provides just a few primitives for static operations.

Essentially, the AVM is a machine-processed standards document. There won't be many AVMs (probably the main line, a few experimental branches, maybe a didactic subset). After Awelon matures, the AVM will evolve very slowly. 

An Awelon application is limited to one AVM. Often, which AVM will be implicit in the development environment, and a compatible AVM can be switched in transparently. (An incompatible AVM can also be switched in; it just breaks your code.) In general, compilers will be limited to a single evolving line of AVMs.

### Primitive Words

A special module called 'avmprim' is created automatically from the AVM. This contains all the primitive words defined in the AVM, and is the ultimate root of all modules in an Awelon project. 

In general, developers do not directly import the primitives module. Primitives are too far removed from the more complicated multi-stack environments presented to developers. Even if they want low-level primitives, developers should use a module that extends them with all the common helpers (which I might name 'pure').

### Awelon Objects

Modules in Awelon are called Awelon Objects or AOs. The extension **.ao** should be used for modules represented in the filesystem (thogh I would like to move Awelon to a more wiki-like environment). AO files should be encoded in UTF-8. The name of the module is simply the filename excluding the extension. If there are ambiguous filenames (e.g. with installed packages), they must be resolved externally.

## Polymorphic Behaviors

In many cases, developers want a word to have a 'higher level' meaning whose precise application depends on context. For example, the word 'copy' should duplicate any element to which it is applied. Awelon has several behaviors that are polymorphic (first, swap, assocl, etc.) but none of them support 'copy' directly because not all types in Awelon can be copied (e.g. linear types cannot be copied). 

Awelon supports ad-hoc polymorphism by introspection of types and static values. 

For example, a 'copy' behavior might look at its argument and decide between a copyStaticText vs. copyStaticInteger primitive. Deep copies, on an `(x*(y*z))` structure or similar, can be achieved by structural induction by use of combinators.

### Static Choice

Awelon supports a concept of static choice `(x | y)` distinct from from dynamic sums `(x + y)`. A static choice is made based on observing a static value or introspecting a static type. A sum may be static or dynamic. Compilers can optimize a static sum under the hood, but developers must assume (with regards to type safety) that a sum is dynamic. Developers have much greater control and awareness of static choice.

Static choice is the basis for ad-hoc polymorphism. Developers can introspect types and values to make decisions:

        %  basic structure exposing primitives
        testIsProduct :: x ~> ((a * b) | x)
        testIsSum :: x ~> ((a + b) | x)
        testIsChoice :: x ~> ((a | b) | x)
        testIsOffer :: x ~> ((a & b) | x)
        testIsNumber :: x ~> (Static Rational | x) %  integer or rational
        testIsIntegral :: x ~> (Static Integer | x) %  just integers
        testIsText :: x ~> (Static Text | x)
        testIsBlock :: x ~> (Static (a ~> b) | x)
        testNumLessThan :: (Static Number * Static Number) ~> (Unit | Unit)
        testLexicalOrder :: (Static Text * Static Text) ~> (Unit | Unit)

More such operations may be available from the AVM. 

Note that programmers cannot test the type of a block. Programmers should *know* the type, much as they would in an untyped language. They can wrap a block with static hints and promises. But they can't actually test for type because blocks can also have ad-hoc polymorphism and there may not be a finite answer to the question, "what are your types?"

Basic operations one might expect for a choice are also supported:

        choiceFirst  :: (Static (x ~> x') * (x | y)) ~> (x' | y)
        choiceAssocl :: (x | (y | z)) ~> ((x | y) | z)
        choiceSwap   :: (x | y) ~> (y | x)
        choiceDist   :: (x * (y | z)) ~> ((x * y) | (x * z))
        choiceIntro0 :: x ~> (0 | x)
        choiceElim0  :: (0 | x) ~> x
        %  etc.

One of the big advantages of static choice is that all-powerful static disjoin primitive. The runtime version of that primitive is much more constrained.

Modeling choice in this first-class manner is much more extensible, expressive, and simpler than passing blocks as arguments. But eventually, developers will want to stop wrapping every operation up with 'choiceLeft'. Since developers are generally aware of which choice they're in, they're free to forget:

        choiceForget :: (x | y) ~> x OR  (x | y) ~> y %  choice no longer visible

After forgetting, developers should either be writing for the correct choice, or writing more polymorphic code that doesn't really care which choice was made. The runtime version of choice is 'merge' but is constrained to have identical types.

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

* dynamic collections (sets, vectors)
* fractional and negative types (right-to-left dataflow)
* linear types (enforcing contracts, protocol completion)
* uniquness source ('create' unique values or exclusive state)
* sealer/unsealer pairs (modeling encryption, signed values, ADTs)
* dynamic behaviors 
* expiration and ripening of dynamic behaviors

### Dynamic Collections

Any general purpose programming language should have effective support for collections - lists or arrays - of both static and dynamic sizes. This is especially true for a reactive model, where there is a big difference between "a signal of arrays" (where the array updates atomically) and "an array of signals" (where each element may update independently). In Awelon, arrays and matrices of static size can be modeled by clever use of products and introspection; I'll leave those to libraries. 

However, primitive support is required for efficient, dynamic collections in Awelon.

Awelon will provide two dynamic collection types: vectors and sets of homogeneous type (which also means all static values must be the same). These collections will be processed by collection-oriented primitives, e.g. foreach operations, or folds for vectors. Vectors are the more expressive, having set operations plus support for order-dependent operations. 

*Note:* I am hoping to avoid 'indexing' into vectors. 

Desiderata: supporting vectors with *coupled* dimensionality. Knowing two vectors have the same size, even if I don't know what is that size, would be very useful for some analyses.

### Fractional and Negative Types

To complete the set of algebraic types `(a * b)` and `(a + b)`, developers gain access to fractional and negative types: `1/a` and `-a`. These concepts are formally described in ["The Two Dualities of Computation: Negative and Fractional Types" by Amr Sabry and Roshan P James](http://www.cs.indiana.edu/~sabry/papers/rational.pdf). 

Fractional types are incredibly useful. With them, one can model:

* adaptive code: polymorphic based on downstream requirements or preferences
* [promises, futures](http://en.wikipedia.org/wiki/Futures_and_promises). One can easily create a few IOUs while working on one subprogram, keep them in hand, and step over to another subprogram to fulfill them. 
* a sort of 'continuation passing' along a pipeline 
* static negotiations between upstream and downstream components

I don't have an intuition or use-case for negative types. But they're symmetric and elegant, so they'll be supported too, and hopefully will find some use someday. 

Awelon introduces just a few primitive behaviors, plus the associated types:

        beat        :: 1 ~> (1/a * a)   
        backbeat    :: (1/a * a) ~> 1
        beat+       :: 0 ~> (-a + a)
        backbeat+   :: (-a + a) ~> 0

Here 'beat' takes the `1/a` as input, and outputs `a`. And 'backbeat' simply goes the other direction. (I getting some rock'n'roll when I named these, but I find them easy to remember.) Awelon is not doing anything clever here; e.g. there is no implicit conversion from `1/(1/a)` to `a`; fractional and negative types are essentially just new wrappers for old types.

A potential issue is that developers can now express invalid data-cycles:

               <-- 1/a --<       THIS IS BAD
              /           \
        beat 1             1 backbeat     
              \           /
               >--- a --->

This would be very problematic if developers use `a` for side-effects! 

Fortunately, these cycles are easy to detect statically. Unfortunately, these cycles are difficult to detect *compositionally*, i.e. in terms of shallow types without global analysis. The main consequence is that fractional types are not supported for dynamic behaviors; they can only be used internally to Awelon's static computation.

Use of beat and backbeat with block composition can also be used to twist inputs to different sides of blocks. The most useful operation is probably just to flip a block around and install it backwards. e.g. `(x~>y)~>(1/y~>1/x)`.

### Dynamic Behaviors



### Static Types


### Type Ascription

Awelon does not have 


### Linear Types

Linear types, from linear logic, are very expressive. Essentially, linear types are types that either cannot be copied or cannot be dropped. By default, all primitive types in Awelon are linear unless specific AVM primitives are provided to copy (`x ~> x*x`) or drop (`x ~> Unit`) them. 

Awelon makes extensive use of linear types to model:

* uniqueness sources
* sealer/unsealer pairs 
* abstract data types
* subprogram responsibilities
* exclusive control of state
* creating unique values
* ad-hoc state models
* resource distribution
* fan-in and fan-out limits

These patterns are discussed in other sections. 

Due to its idempotence, RDP has difficulty expressing exclusivity without linearity. Two RDP behaviors each asking for the OOP equivalent of a `new Dog()` would necessarily receive the same dog. The two subprograms could distinguish their dogs - one asking for `new Dog("fido")` and the other asking for `new Dog("spot")`. But that doesn't guarantee exclusivity. Linear elements address this problem: we can ask for a `new(uniqueSource) Dog()`, and guarantee that our new dog is unique - and, consequently, that we initially have exclusive control over its state.

A feature valuable for live programming, debugging, and orthogonal persistence is that state will be stable across restarts and changes in code. In Awelon, developers must use the aforementioned techniques together, i.e. using "fido" vs. "spot" to stabilize the relationship between external storage and the code. This is achieved by 'forking' a linear source with static values, assigning different subprograms to different subtrees in a stable manner. The AVM enforces this for most primitive linear operations.

Most linear computations in Awelon are static. Static blocks can be wrapped to become linear static blocks, enforcing that they be used exactly once. 

*NOTE:* RDP behaviors cannot output unique values unless a uniqueness source is also an input. Unique sources are necessarily linear. RDP behaviors can, however, output non-unique linear values, which simply model 'must use' contracts within the source code.

### Capabilities

If you are not familiar with the concept of object capability security, I recommend reading [Ode To Capabilities](http://erights.org/elib/capability/ode/ode-capabilities.html). Capabilities are an excellent way to manage authority, being very expressive and having most properties of [secure interaction design](http://zesty.ca/pubs/csd-02-1184.pdf). In context of RDP, capabilities are essentially behaviors passed as arguments to other behaviors. RDP is designed for distributed systems with runtime code-distribution between mutually distrustful systems; avoiding ambient authority is a very good idea. Capability security has been part of RDP's design from the beginning.

In Awelon, capability security can be enforced by eliminating [ambient authority](http://en.wikipedia.org/wiki/Ambient_authority) behaviors (e.g. effectful primitives in the AVM) and shifting authorities to application parameters. Capabilities in Awelon may be *linear*, which can further enforce certain security properties. Capabilities can be *static* (and usually are, in Awelon), which is very nice for optimization purposes. 

A good place for a programmer to store capabilities is the programmer's hands (part of Awelon's default tacit environment). By default, the contents of those hands are not passed to partially applied subprograms. This supports an intuition that it's the programmer holding the keys, and carefully granting authority to untrusted subprograms. 

But we can do better! 

#### Mixed Capability and Ambient Authority

With a carefully designed AVM, Awelon can easily support a *mix* of ambient authority and capability security. Each host can support two or more kinds of partitions: one with ambient authority, one without, and asymmetric communication between them. Distrusted developers can be forced to target the secured variation of the partition, but their code will receive capabilities that reach back into the ambient authority partition. 

There are at least a few advantages to this mixed design:

* More uniform application model from compiler's perspective.
* Developers have freedom to introduce new ad-hoc capabilities when necessary.
* Ambient authority, for all its insecurity, is very convenient.

Developers can create their own ad-hoc application models by essentially wrapping the main behavior.

        @appModel
        import frameworkStuff
        this = gatherFooCaps enterHighestSecurityMode runFooApp
        @app
        import appStuff
        this = meh blah boring %  assuming access to FooCaps

Instead of compiling `app` directly, then, developers simply compile `[app] appModel`.

This is the design that Awelon favors. 

#### Coupling Power and Responsibility

Linear types allow an API to say what a subprogram *must* do, and capabilities allow an API to say what a subprogram *can* do. They can be combined by requiring linear arguments to a capability, or by use of linear capabilities. This combination can be very expressive, while still supporting a great degree of flexibility in how things are achieved.

### Modeling ADTs and Objects

Awelon programmers don't have user-defined types per se, but they can model ad-hoc types. 

By simple use of sealer/unsealer pairs, programmers can model abstract data types (ADTs) and objects or existential types. The idea in both cases is that we have sealed values (representing the ADT or object) along with a collection of named behaviors that will unseal the value, operate on it, seal it back up, and maybe return something. An ADT differs from an object simply by separating the 'collection of operations' from the sealed values.

In Awelon, these would be reactive dataflow objects and ADTs, consisting of runtime signals. The set of objects in an Awelon application would generally be static, but they can still be useful.

#### Parametric Polymorphism

Awelon's design favors ad-hoc polymorphism; a behavior is free to specialize or generalize based on knowledge about types. (Ad-hoc polymorphism may serve better for heterogeneous computation systems.) But there is also utility in parametric polymorphism - enforcing a uniform structure for a given class of types. Modeling ADTs or objects can easily enforce parametricity.

## Awelon Language Primitives

### Debugging and Live Programming Support

Warnings, Deprecations, Gauges.

Due to its support for static types, Awelon code can readily be augmented with 'active' documentation - e.g. commands that don't do anything except indicate how to display or debug the code itself. 

### Collections Oriented Programming

### Reverse Dataflows

beat and backbeat
every signal augmented with direction (the signals that can be, anyway)
every signal augmented with beat#

           S1 ~> (CoStatic N x :&: Static N x) %  receive costat, output stat
           (CoStatic (N - 1) x :&: Static N x) ~> S1 %  receive stat, output costat


### Distributing Data

Partitions, disruption, type conversions... 







