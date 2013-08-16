
# Awelon

Awelon is a richly typed, compiled language for reactive demand programming (RDP). RDP is a reactive dataflow model designed for open distributed systems (more in the docs). Awelon is intended to be a usable programming language for humans, a feasible distribution language for RDP code and software components, and a viable target for RDP development environments (some of which might be more visual in nature).

This Haskell project will provide a compiler for Awelon, to at least bootstrap it.

## Tacit Programming

Awelon code is [tacit](http://en.wikipedia.org/wiki/Tacit_programming). Juxtaposition is composition. All inputs are implicit, in an environment. However, unlike most tacit languages, Awelon's environment exists only as a compile-time type, and all manipulations to it are pure. Developers use the environment at a *conceptual* level - a way to organize resources and code, to envision or specify what a program should be doing. 

At runtime, the environment is gone and the data plumbing is compiled completely away.

### Stack Based Programming

Like many tacit programming languages, Awelon supports stack-based programming. One of the key aspects of tacit programming is how *literals* are handled - e.g. if a developer writes `10 11 12`, what happens? Somehow, those literals must be added to the environment. For example, by pushing it onto a stack.

        42 -- e ~> (Static Integer * e)
        12.3 -- e ~> (Static Rational * e), exact 123/10
        "literal string" -- e ~> (Static String * e)
        [swap] -- e ~> (Static ((a * b) ~> (b * a)) * e)

In Awelon, literals include integers, rationals, strings, and blocks (of code). I would like to support collections using `{}` (but I haven't decided how). Literals have 'Static' types, meaning their values are available at compile-time both for operations and decisions. Static types also are the key ingredient to make tacit programming work with an arrowized model, enabling us to model 'first' without use of explicit parameters.

        -- first :: (x ~> x') -> ((x*y) ~> (x'*y))              HASKELL
        -- first :: (Static (x ~> x') * (x * y)) ~> (x' * y)    AWELON
        [swap] first -- ((a*b)*c) ~> ((b*a)*c)

Support for partial application with 'first' is valuable for both code reuse and expressiveness. Awelon's model for 'first' has greater expressiveness because it allows the specification of the behavior to be cleanly separated from its call site, thus avoiding the need for an orthogonal abstraction model (such as lambdas). There is a price: Awelon's first is expressive enough to cause trouble. The traditional non-terminating lambda (`(\x->(x x) \x->(x x)`) can be represented (`[copy apply] [copy apply] first`).

To help illustrate what tacit code often looks like, I'll define a few more functions:

        -- assocl :: (x * (y * z)) ~> ((x * y) * z)         PRIMITIVE
        -- swap   :: (x * y) ~> (y * x)                     PRIMITIVE
        -- intro1 :: x ~> (Unit * x)                        PRIMITIVE
        -- elim1  :: (Unit * x) ~> x                        PRIMITIVE
        -- copy   :: x ~> (x * x)                           polymorphic; see below
        
        apply  = [intro1 swap] second first swap elim1      -- (Static (x ~> x') * x) ~> x'
        second = swap [swap] first swap first swap          -- (Static (y~>y') * (x*y)) ~> (x*y')
        assocr = [swap] first swap assocl [swap] first swap -- ((x * y) * z) ~> (x * (y * z))
        roll2  = assocl [swap] first assocr                 -- (x * (y * z)) ~> (y * (x * z))
        roll3  = [roll2] second roll2                       -- (a * (b * (c * d)))) ~> (c * (a * (b * d)))
        roll4  = [roll3] second roll2
        unroll2 = roll2
        unroll3 = unroll2 [unroll2] second                  -- (c * (a * (b * d))) ~> (a * (b * (c * d)))
        unroll4 = unroll2 [unroll3] second
        dup    = [copy] first assocr                        -- (a * e) ~> (a * (a * e))
        pick1  = [dup] second roll2                         -- (a * (b * e)) ~> (b * (a * (b * e)))
        pick2  = [pick1] second roll2

Tacit code is easy to incrementally parse and process, and similarly easy to generate. Those are useful properties for Awelon's goals as a distribution or target language. Tacit code is naturally composable, and should be effective for representing software components. Avoiding direct manipulation by names enables many of RDP's invariants to be enforced constructively. Tacit code has very little syntactic noise. 

However, tacit code has a weakness: much of the data plumbing becomes explicit. Data plumbing words become a form of semantic noise, unrelated to the problem domain. For productive development, it is necessary that these data plumbing operations become simple, intuitive, fading into the background of the developer's attentions. 

*ASIDE:* Traditional languages also have difficulty with data plumbing - it just happens in higher layers: concurrency, callbacks, collections. RDP is designed to address many challenges in those higher layers.

Data plumbing on a stack is relatively simple: developers mostly operate near the top of the stack, and occasionally roll or copy elements to the top. It does not take long to become familiar with these manipulations, and stacks are effective for linear operations. Unfortunately, a single stack is inadequate for representing incremental tasks or concurrent workflows, especially if there is some interweaving of intermediate results. Also, there are cases where devilish data plumbing problems on a single stack would be solved trivially with just a little extra scratch space.

### Multi Stack Environment

Awelon's static environment contains a non-empty *list* of stacks. There is always a current stack, and all the single stack operators now apply to the current stack. Developers can add new stacks, drop old stacks, and navigate between them. These additional stacks quickly find use by developers: different stacks for different tasks, scratch spaces, temporary storage, and that stuff you never get back to but keep thinking you might. 

        -- List of Stacks:  (stacksLeft * (currentStack * stacksRight))
        -- stepLeft  :: ((sT * sL) * (sC * sR)) ~> (sL * (sT * (sC * sR)))
        -- stepRight :: (sL * (sC * (sT * sR))) ~> ((sC * sL) * (sT * sR))
        stepLeft = [swap] first assocr
        stepRight = assocl [swap] first [assertIsProduct] second

        -- New, dup, and drop for stacks create or destroy a stack to your right
        --
        -- newStack  :: (sL * (sC * sR)) ~> (sL * (sC * (Unit * sR)))
        -- dupStack  :: (sL * (sC * sR)) ~> (sL * (sC * (sC * sR))) -- if non-linear sC
        -- dropStack :: (sL * (sC * (sD * sR))) ~> (sL * (sC * sR)) -- if non-linear sD  
        newStack = assocl [swap] first [intro1] second
        copyStack = [[copy] first assocr] second
        dropStack =  [[dropFirst] second] second
       
        -- more basics
        erase   :: x ~> Unit        -- for non-linear x
        dropFirst = [erase] first elim1  -- (e * x) ~> x, for non-linear e
        dropSecond = swap dropFirst      -- (x * e) ~> x, for non-linear e

Of course, multiple stacks are useless if we don't have an effective way to move elements between them. I could potentially model throwing things between stacks, but there is a better way.

### Hands to Carry Things

Awelon's multi stack environment is further extended with the concept of *hands*. With hands, a developer can take elements from one stack, navigate to another, then put or apply them. Hands are also convenient for operations on a single stack, i.e. rather than rolling stuff to the top, just pick up the top several elements and do some work, then put those several elements back down.

Overall, hands offer a simple, intuitive, and expressive approach to data plumbing. 

Hands are modeled as a pair of stacks, and paired with the current environment, such that our overall environment is a structure of the form: `(sL*(sC*sR))*(hL*(x*hR))`. Here current stack, and x is the active object - if any. Unlike the list of stacks, the developer's hands may be empty.

        -- take :: (sL*((x*sC)*sR))*(hL*hR) ~> (sL*(sC*sR))*(hL*(x*hR))
        take = [[assocr] second roll2] first assocr roll2 [roll2] second

By convention, only the right hand is used for volatile operations and world interactions. The left hand is used instead for environment extensions based on introspection and metaprogramming - such as modeling named stacks.

*NOTE:* The programmers hands are generally NOT passed to partially applied behaviors. The intuition here is that the developer is installing a behavior that will then execute without further guidance or interference. Also, conversely, the installed behavior must not affect the contents of the programmer's hand. Arguments to a partially applied behavior must be explicitly provided. 

### The Literals Brush

Thus far, I've modified the environment from a single stack, to a multi-stack, to a multi-stack with hands. Each time, I have needed to adjust how literals are added to the current stack. If developers wish to create an alternative environment, they would also need to adjust how literals are introduced. Other words can be adjusted through the module system. Other literals need special attention.

To address this concern, a literals brush (lb) is added to the environment. We now have `(lb*env)`. Whenever a literal is added to the code, the literals brush is copied (it must be a static block) then applied to the resulting `(literal*env)` pair to determine the new environment. 

Let's consider how this literals brush would have evolved over the last three environments:

        -- single stack environment
        -- (x * e) ~> (x * e)
        lbSS = id

        -- multi-stack environment
        -- (x * (sL * (sC * sR))) ~> (sL * ((x * sC) * sR))
        lbMS = rot2 [assocl] second

        -- multi-stack environment with hands
        -- (x * ((sL * (sC * sR)) * (lH * rH)) ~> ((sL * ((x * sC) * sR)) * (lH * rH)
        lbHMS = assocl [rot2 [assocl] second] first assocr

Developers shouldn't think about the literals brush unless they're planning an environment extension. It is also possible to use the literals brush for some modal operations, which might prove useful for modeling DSLs.  Naturally, the literals brush must be highly polymorphic.

### Named Stacks

Awelon supports storing and loading elements by name. Use of names is a valid and convenient solution for many data plumbing problems: simply tuck a value away with an arbitrarily chosen name, then later recover it using the same name. Names are easier to remember than relative locations. 

Names are modeled by use of polymorphic, introspective behaviors (see later section) that will search the left hand for a named stack, then add or remove an item (adding or removing the stack as appropriate). The use of stacks can serve a similar role to lexical scope, though developers must be careful to erase names when they exit logical scope. 

        "foo" store    -- store element to "foo" stack (removes from current stack)
        "foo" load     -- load element from "foo" stack (removes from foo stack)
        "foo" loadCopy -- load, dup, store 
        "foo" erase    -- load, drop
        -- possibly loadStack and storeStack operations, too.

The default movement semantics are suitable for linear types, or cases where developers wish to model updates. (These variables are 'pure' from Awelon's perspective.) Unlike traditional programming, developers must explicitly erase words th

Names can become syntactic noise and clutter if overused. I prefer to use names sparingly, for long-lived elements that spend most of their time in the background. Other programmers may prefer more extensive use.

## Awelon's Module System

Awelon has a very simple module system. Essentially, a module consists of one import line followed by one or more definition lines. An import line contains a comma separated list of module names. The association between a name and a module is externally determined (e.g. by file name) and should be simple alphanumeric words. Modules may be associated with local names. Imports must be acyclic. 

        import common, packageWithLongName AS p, foo
        _s = swap [swap] first swap first swap
        f = foo [p:bar foo:bar] first -- here 'foo' means 'foo:this'
        this = [f] _s

Words within a module must be defined before they are used. All words defined in a module are exported from it, except those prefixed with the underscore character such as _s above. (Imported words are not re-exported.) All words are also available prefixed with 'module:' from which they come. If there is any potential ambiguity for a word (i.e. if it is exported from two of the modules, including this one), it becomes necessary to use the prefixed form.

The word 'this' has special meaning. Within a module, 'this:' may be used as the prefix for a module's words. When imported, the word 'this' is mapped instead to the import name, e.g. 'foo' means 'foo:this' after importing foo (assuming no ambiguity). Also, the word 'this' in Awelon serves the same role as 'main' in many other languages. Awelon is intended for component based software; I believe 'this' has nice connotations for treating modules as possible software components.

All imports and definitions must start at the beginning of a new line of text, and continue to the end of the logical line. A logical line continues until a non whitespace character begins a new line of text, i.e. so developers can distribute some code or imports vertically. Awelon has line comments in Haskell's style, starting with `--` (as a distinct word). 

        import common
        -- take :: (sL*((x*sC)*sR))*(hL*hR) ~> (sL*(sC*sR))*(hL*(x*hR))
        take = [[assocr] second roll2] first -- (x*(sL*(sC*sR)))*(hL*hR)
               assocr roll2 [roll2] second

Note: there are no default words or imports in Awelon. Only literals can be used without an import.

*NOTE:* I believe developers shouldn't need more than a line of boiler plate before they get to useful work, which is why I allow multiple imports on one line. I discourage deep, hierarchical module systems. Libraries should generally be exported at the package layer. Ambiguity is rare in practice, and resolved easily enough. Modules as software components are encouraged to export 'this' and little else.

### Awelon Virtual Machine

The Awelon Virtual Machine (AVM) is a description of an RDP-based programming environment. 

This description includes types, partitions, and behavior primitives. It is a complete description, repeating even the ‘standard’ primitives like first and compose. Unlike a traditional VM, an AVM can describe heterogeneous systems, where different partitions have different behavior primitives and asymmetric communication. An AVM may additionally include: equational laws, proposed rewrite rules, application models. An application model would describe behavior types for applications or pluggable applets, along with some English text about how it is used. 

The AVM is represented as an Awelon module that operates in a restricted subset of Awelon to construct its description (i.e. as opposed to using JSON or XML). It can import a special module named 'avmbs' (AVM bootstrap), which the parser understands implicitly, and other modules that have the same limit. The avmbs module supports a small, static subset of standard behaviors. An argument to the AVM is a behavior that is applied to each definition as it is constructed.

Essentially, the AVM is a machine-processed standards document. There won't be many AVMs (probably the main line, a few experimental branches, maybe a didactic subset). After Awelon matures, the AVM will evolve very slowly. 

An Awelon application is limited to one AVM. Often, which AVM will be implicit in the development environment, and a compatible AVM can be switched in transparently. (An incompatible AVM can also be switched in; it just breaks your code.) In general, compilers will be limited to a single evolving line of AVMs.

### Primitive Words

A module is created, with the arcane name avmp (AVM primitives), which exports all the primitive words from the AVM. 

        import avmp,...

In general, developers do not directly import the primitives module. Primitives are too far removed from the more complicated multi-stack environments presented to developers. Even if they want low-level primitives, developers should use a module that extends them with all the common helpers (which I might name `pure`).

### Multi-module Files

*Recommendation*

There are cases where representing multiple modules in a single document or stream is useful. In this case, the recommendation is that each module starts with an `@moduleName` header, on a new line of text.

        @lbms -- literal brush for multi-stacks
        import pure
        this = assocl [rot2 [assocl] second] first assocr
        @bar
        import foo, common
        ...

Modules still don't know their own names, but they'd be pretty obvious. 

## Polymorphic Behaviors

In many cases, developers want a word to have a 'higher level' meaning whose precise application depends on context. For example, the word 'copy' should duplicate any element to which it is applied. Without some form of polymorphism, a developer might need to use separate operations - e.g. 'copyStaticString' vs. 'copyRuntimeInteger' - and it would be infeasible to develop copy operations for ad-hoc `(x*(y*z))` types without primitive support. But polymorphic *primitives* are in many cases undesirable, because they assume too much; it is easy to overlook exceptions (such as linear types, which are uncopyable).

In Awelon, polymorphic behaviors are achieved by compile-time introspection of static inputs and types, and selecting a program based on these goals. In case of recursive structural operations (e.g. copy, drop), developers must utilize a fixpoint combinator.

**NEEDED** copy, drop examples; K pick/roll; 

### Anonymous Recursion in Awelon

Awelon does not support recursive definitions. Sadly, this means most people will never fully grok recursion in Awelon. However, recursion isn't all that common in Awelon.

        Yv = λf.(λx.f (λv.((x x) v))) (λx.f (λv.((x x) v)))
        Θv = (λx. λy. (y (λz. x x y z))) (λx. λy. (y (λz. x x y z)))

### Sloppy Programming


### Aspect Oriented Awelon

IDEA: use a `"foo" signal` to represent a join-point named 'foo'. The signal operation will then search the left hand for appropriate advice. This is a bit weaker than true AOP, since it requires developers specify the join points; but it is still very powerful.




## Awelon's Types

0) static errors, warnings
1) how to handle 'static' linear behaviors.
2) how to handle 'static' capabilities effectively.

### Linear Types

Uniques, Sealer/Unsealer pairs, 
Source-Stable Local State (with linear capability + source-stable string identifiers)
Resource distribution and control... (even stuff like screen real-estate, observation for keys?)
Linearity tags

### Capabilities

If you are not familiar with the concept of object capability security, I recommend reading [Ode To Capabilities](http://erights.org/elib/capability/ode/ode-capabilities.html). Capabilities are an excellent way to manage authority, being very expressive and having most properties of [secure interaction design](http://zesty.ca/pubs/csd-02-1184.pdf). In context of RDP, capabilities are essentially behaviors passed as arguments to other behaviors. RDP is designed for distributed systems with runtime code-distribution between mutually distrustful systems; avoiding ambient authority is a very good idea. Capability security has been part of RDP's design from the beginning.

In Awelon, capability security can be enforced by eliminating [ambient authority](http://en.wikipedia.org/wiki/Ambient_authority) behaviors (e.g. effectful primitives in the AVM) and shifting authorities to application parameters. Capabilities in Awelon do not need to be dynamic; they may be static, linear, or both. 

These behaviors may be statically computed. But they aren't

In Awelon, a good place to keep capabilities is the *hand*, which is not automatically passed to partially applied subprograms. The developer will need to explicitly model fetching capabilities from the hand to pass to a subprogram. 




Capabilities vs Ambient Authority
=================================

Ambient authority is authority based on the context in which code is executed - e.g. which partition, who is running the process. Sadly, ambient authority is not very expressive or precise for security policies, and is difficult to control (since it's invisible). A preferable design is object capability model, where authority is granted through explicit parameter and threaded explicitly through an application. There is also an intermediate form of authority, e.g. where a key or certificate is threaded through a program (with an intermediate level of expressiveness; keys do not easily model attenuation).

AVM can express any of these possibilities. If resources are made available as toplevel behaviors, you'll have ambient authority. If resources are provided as parameters to the target application model, then you'll have capability security. Sealer/unsealer pairs (which themselves are resources) can model the intermediate option.

For Awelon, I'm not entirely sure which will be favored. Ambient authority has its advantages, for convenience, and a reasonable level of security can likely be achieved via heterogeneous partitioning (e.g. secure vs. insecure partitions). Awelon's module system is relatively *flat*, which hinders deep cross-module uses of parameters (except indirectly, via dynamic behaviors).


### Modeling ADTs

Due to introspection, Awelon does not have parametricity. Any behavior can inspect a complex value for its structure. However, developers can enforce parametricity by use of sealer/unsealer pairs.

## Awelon Language Primitives

### Debugging and Live Programming Support

Warnings, Deprecations, Gauges.



Awelon has some additional features to support "smart" data plumbing, and also higher levels of thinking about programs. 



In Awelon, the 'stack' is a is  entity. Not even an entity. It's a way of thinking about the program's type. On one hand, these data plumbing operations are *free* at runtime. On the other, developers cannot manipulate the stack using dynamic data. 

Awelon's encoding makes it very easy to parse. An advantage of this encoding 


Advantages of this encoding style are that it is very easy to parse, and highly composable. A disadvantage, OTOH, is the number of explicit data plumbing operations in the code. (Fortunately, developers don't need to worry about runtime costs

 of this coding style is that it is highly composable in a consistent manner. A disadvantage, OTOH, is the number of data plumbing operations that become sprinkled throughout the code. 

In Awelon, these operations will tend to be free at runtime (since the stat


A standard library of functions will eventually be developed.

A standard library of helper functions will eventually exist. 

At runtime, all this data plumbing tends to be free. 

The above shows what some Awelon code might look like - just a series of words, occasionally quoting some code. In some ways, Awelon is much simpler than FORTH because Awelon always has a static-sized signal stack. 

        

*Notes:* Type ascriptions (use of `::`) are not a valid part of Awelon code. They're here for documentation purposes only. Types are excluded entirely from Awelon code, though developers do have some ability to assert attributes of a type.


Above, `Static` is a special signal type, indicating a value that is available at compile-time. In the right conditions, a static signal can be lowered into the runtime. 


Note: the ascriptive `:: type` tags here are not part of Awelon code. If developers wish to assert that a certain type is used at a given point in the code, this can be achieved using typed identity behaviors. 

Note the left hand `x` will (in practice) generally be a complex signal of a form similar to `(a & (b & (c & ...)))`. Further, each of `a`, `b`, `c`, etc. may also be a complex signal, consisting of `*` types or `+` types. 

Awelon's primitives are designed with this in mind. 



There are no parameters, not even for templates, but developers can model compile-time parameters in terms of 'static' signals. 

        

 signals. 
juxtaposition as composition, no use of parameters. However, t


This actually operates in a manner similar to the FORTH stack, though the stack elements can have arbitrary complex signal types. 

Awelon has a rich type system, and type-driven search can be used to statically compute parts of the program.



An Awelon system consists of several elements:
* An Awelon Virtual Machine (AVM) definition - a documented set of typed primitives and equational laws
* Awelon Object (AO) code - implements an RDP behavior of some type, application or component
* Compilers and optimizers to implement Awelon on modern operating systems and architectures

An RDP application will start in some partition with a 'go' signal, but after that point code might need to be distributed to remote systems. In some cases, e.g. if distributing JavaScript/DOM or CUDA code, a compiler might be able to process those parts in advance so they're prepared for distribution. But a goal is that some remote services will support Awelon code directly (i.e. as a basis for installing user-agents), which should have some advantages in terms of type checking, optimizability, and security.

It will be a while before many resources are supported by compilers. So, in the interest of getting something useful, Awelon's compilers will initially target web apps and web app servers. 

### Active Documentation

Due to its support for static types, Awelon code can readily be augmented with 'active' documentation - e.g. commands that don't do anything except indicate how to display or debug the code itself. 


### Dependent Types

### Linear Types

### Collections Oriented Programming


### Reverse Dataflows

beat and backbeat
every signal augmented with direction (the signals that can be, anyway)
every signal augmented with beat#

           S1 ~> (CoStatic N x :&: Static N x) -- receive costat, output stat
           (CoStatic (N - 1) x :&: Static N x) ~> S1 -- receive stat, output costat


### Distributing Data

Partitions, disruption, type conversions... 



# 

An Awelon system consists primarily of three elements:

* the Awelon Virtual Machine (AVM) definition file, which documents the heterogeneous distributed machine
* the Awelon Object (AO) files, which describe RDP behaviors representing applications or reusable components
* a set of compilers and optimizers, which operate on AOs and ultimately generate executable code

Awelon itself defines the type system and how to parse and understand AVMs and AOs. 

The AVM describes and documents primitive behaviors, their types, equational laws, proposed rewrite rules, standard definitions (a prelude), and common target types for applications. In practice, there will be very few AVM files in development, and they will evolve slowly after reaching maturity. One might consider the AVM to be a machine-readable standards document. 




 Composition as juxtaposition. Literals are treated as behaviors, and 

Any parameters are modeled as signals, if they're needed at all. Compile-time parameters are modeled as *static* signals. Literals are modeled as behaviors that trivially add a static signal to the environment. 


Awelon does support *quoting* of code, i.e. to treat a behavior as a value, by simply wrapping the code in square brackets. Similarly, literals are modeled as behaviors.

In Sirea, several behaviors were parameterized, with `first` being the most common example. 

Developers can use 

Developers can use local definitions, and there is also a means to quote code. 

Code looks like a long sequence of words, and potentially a few local definitions. 


Awelon code does not support type descriptions, rather there is an external 'Awelon B


behaviors are not parameterized (though static signals can serve a similar role). 

Awelon Code
-----------


There are a few special cases: 

* One can quote code by wrapping it in `[]` brackets. This allows developers to treat code as literals.
* One can include a literal string at any time, using `''` or `""`. The 
* Basic number literals are also supported - e.g. integers, floats. 

        

, in a manner similar to FORTH or J. And an AO essentially consists of a sequence of local definitions followed by the main behavior. 

- a sequence of behaviors with occasional quoting. 



Every AO actually specifies a non-empty *set* of behaviors. Which behavior is selected is type-driven. This enables AO components to perform some intelligent auto-wiring, or to adapt flexibly to different usage contexts. An AO is a complete behavior, i.e. no missing definitions or external dependencies. If developers must have linking, they can model it actively: in terms of behaviors that search shared repositories or publish to them. 



. If developers wish to model ad-hoc linking, this must be represented more actively - i.e. in terms of a behavior that will publish resources to a shared registry or search for resources in the same.

One way to make software more reusable is to extract hard-coded values and make them user-definable. In Awelon, this can be accomplished by use of 'static' signal types - i.e. signals that are guaranteed to have a fixed value at compile time. 



Awelon does not have parameters per se, but developers

If an AO should be parameterized, that can be modeled by use of static input signals.
An AO cannot be 'parameterized', but it might receive static signals. 

there are no holes to fill, no external linker for dependencies. (That said, some 

Awelon has no *external* linking model to fill dependencies (as having one would interfere with code distribution). That said, some Awelon application types actively might search their environment for useful resources, or publish resources in turn. 

 Each AO represents a *set* of potential RDP behaviors, 



An AVM describes the primitive behaviors and their types, equational laws, proposed rewrite rules, and standard tactics. The AVM may also contain useful English descriptors for each primitive. In practice, there will be very few AVM files, and they will tend to evolve slowly after reaching maturity. The AVM is the only location that Awelon's type language is used explicitly.

The AO is the basic unit of Awelon code. Each AO describes a *set* of potential RDP behaviors. From that set, one behavior will be statically selected. The decision is type-driven, sensitive to both the required input and output types, enabling Awelon code to be adaptive and context sensitive, and potentially more reusable. The set of behaviors is represented as a weighted grammar that can generate multiple behaviors of different types. 

Every AO is *self-contained* and *complete* - no holes to fill, no external linker. These properties are valuable for code distribution, and for sharing or distribution of reusable component-oriented software. That said, the traditional role of parameters can be achieved by use of 'static' signals. Also, linking can be modeled internally, i.e. in terms of actively searching a link-time environment (or publishing resources to it) in a staged application model.

Compilers or optimizers may be specialized to an AVM or application type. Initial efforts will target rich web application servers... then cloud, mobile, or gaming. This Awelon project in Haskell defines some of these initial compilers. 





http://en.wikipedia.org/wiki/Tacit_programming

 (e.g. type safety, linear types, dependent types, [negative and fractional types](http://www.cs.indiana.edu/~rpjames/papers/rational.subc.pdf), tacit programming). 


RDP does not need traditional loops; recursion patterns are better addressed by 'foreach' operations 







This code will represent an RDP behavior, or a set of behaviors selected by static type. A single AO may describe behaviors that cross multiple heterogenous 'partitons' - as between client and server, or CPU and GPU. Each partition may potentially have its own set of primitive behaviors, types, and resources. 

An AO may serve as an application or a reusable component. An AO cannot directly depend upon other AOs

 different kinds of applications depending on the starting partition and input types. 

 each partition may have its own resources.

  as between client and server, or CPU and GPU. 

A single Awelon Object (AO) - a component, represented in a file or string - may describe behaviors that are distributed across multiple components.


An AO is a complete component, there is no implicit notion of external dependencies (though a particula

An AO will be further compiled - i.e. to a mix of JavaScript and machine code - to execute on available architectures. 

Awelon 

An Awelon Object (AO) can serve as a component in a component-based system. An Awelon *application* is simply a component that has the right type to operate 


Awelon is not intended directly for human use, though that usage is not prevented. 

 must be further compiled, e.g. to machine code, or LLVM, or JavaScript, or a mix of these in order to execute on modern systems. However, an AO



RDP is a reactive, declarative dataflow programming model that allows access to effects. RDP is designed to support large scale distributed systems and heterogeneous computation environments. a single Awelon object can describe both server and client-DOM behavior, or include code for both CPU and GPU.



 kinds of code for GPU vs. CPU. 

, and is carefully designed to support large scale distributed systems... and potentially *heterogeneous* systems (e.g. integrating GPU, FPGA, HTML client, server side, etc. code into one object)

  declarative, reactive, dataflow programming model 

suitable for heterogeneous systems (e.g. defining client and server code in one program), distributed systems, open systems. 

 with constrained effects (in particular, effects are commutative and idempotent). 

Traditionally, low-level languages are procedural or imperative, close in nature to CPUs. 

The role of Awelon is the specification of safe, distributable "Awelon Objects" that can l

Awelon is a general purpose programming language based on reactive demand programming (RDP). RDP is a highly declarative, bidirectional dataflow model suitable for orchestration in open, distributed systems. To RDP, Awelon will add a rich type system, effects systems, external state models, a module system, and a distribution model. In addition, Awelon supports ad-hoc syntactic abstraction per module.







