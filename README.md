
# Awelon

Awelon is a richly typed, compiled language for reactive demand programming (RDP). RDP is a reactive dataflow model developed for handling open distributed systems (more in the docs). Awelon is intended to become a primary distribution language for RDP code and software components, and a viable intermediate target for RDP development environments (which might not be textual).

## Tacit Programming

Awelon code is primarily [tacit](http://en.wikipedia.org/wiki/Tacit_programming). Juxtaposition is composition. Parameters are modeled as input signals, which are implicit in the environment. The environment can often be conceptualized as a 'stack of signals' of a form similar to `(x * (y * (z * ...)))`. Literals are added as statics to the top of this stack in a manner reminiscent of FORTH (albeit, flexibly typed). The literals of Awelon will include numbers, strings, and blocks. A block is a subprogram captured as a value by wrapping it with square brackets.

        42 -- e ~> (Static Int * e)
        "literal string" -- e ~> (Static String * e)
        [foo] -- e ~> (Static (y ~> z) * e)

To operate on just part of the 'stack' typically involves wrapping a subprogram in a block on the stack, then applying it selectively. This is supported by a few primitives, and a large variety of data-plumbing helper functions.

        head :: (Static (x ~> y) * (x * z)) ~> (y * z) -- prim
        assoclp :: (x * (y * z)) ~> ((x * y) * z) -- prim
        swap :: (x * y) ~> (y * x) -- prim

        -- tail :: (Static (y ~> z) * (x * y)) ~> (x * z)
        tail = swap [swap] head swap head swap

        first = head
        second = [first] tail
        third = [second] tail
        fourth = [third] tail

        -- assocrp :: ((x * y) * z) ~> (x * (y * z))
        assocrp = [swap] first swap assoclp [swap] first swap

        -- rot2 :: (x1 * (x2 * e)) ~> (x2 * (x1 * e))
        -- rot3 :: (x1 * (x2 * (x3 * e))) ~> (x3 * (x1 * (x2 * e)))
        rot2 = assoclp [swap] first assocrp
        rot3 = [rot2] tail rot2

        -- 10 11 rot2 == 11 10
        -- 10 11 12 rot3 == 11 12 10

For small subprograms (i.e. the bulk of most applications), position-based parameters work very well. However, this technique doesn't scale. There are at least two significant concerns. First, beyond about five items, humans have difficulty tracking by position. Second, large input sets tend to grow larger: e.g. if you have ten inputs, why not eleven? Programs developed against relative position are fragile to extension. For complicated environments, developers should have more extensible and memorable options: symbolic names, keyword parameters, and the like. Fortunately, Awelon can provide.

## Type Driven Programming

Awelon supports a simple but expressive form of type-driven programming: overloading. Each word in Awelon may be given multiple meanings. These different meanings may have different types. At compile time, one meaning is selected for each use of a word such that the application passes all static checks - i.e. typechecking and static assertions. 

This technique enables Awelon developers to express adaptive software components, robust to extension, reusable in more contexts. It also is the basis for generic programming in Awelon. As a simple case study, consider dup, defined in terms of its primitives and some type-driven search:

        dupStatic :: (Static x) ~> (Static x * Static x) -- prim
        dupAtomic :: (Atomic p dt x) ~> (Atomic p dt x * Atomic p dt x) -- prim

        -- zip2 :: (x1 * x2) * (y1 * y2) ~> (x1 * y1) * (x2 * y2)
        zip2 = assocrp rot3 rot2 assoclp

        dup =| dupStatic  
        dup =| dupAtomic
        dup =| [dup] head [dup] tail zip2

        -- copy corresponds to FORTH's dup
        copy = [dup] first assocrp

With this definition, dup can be applied to both primitives and to complex `*` signals. Of course, this definition is incomplete: it doesn't handle `+` types at all. Fortunately, overloaded definitions support *open extension*: extensions may be applied to an imported definition, and will automatically be integrated to any recursive structure. A developer who later sees this missing feature may transparently introduce it.

*NOTE:* Definitions may only be overloaded if they're defined with `=|`. This constraint exists for visibility reasons: developers should know at a glance whether or not they see a complete definition. Also, only `=|` definitions may be recursive.

### Keyword Arguments in Awelon

Awelon's overloading is expressive enough to represent special variables, keyword arguments, extensible records, and so on. Usefully, it's all free at runtime! What we represent here is a compile-time computation that builds the data-plumbing to operate on a particular element.

The first thing we must do is decide how to represent these associations. There are actually quite a few factors one might consider: syntactic convenience, ease of manipulations (add key, rename key, remove key, drop by key, operate on signal, etc.), effective support for hierarchical representations (e.g. a record of records), and so on. One of the easier to manipulate representations is simply to use a list of `("keyword",argument)` pairs. I.e. instead of `(x * (y * (z * e)))` we might have an assoc list similar to `(("kw1" * x) * (("kw2" * y) * (("kw3" * z) * e)))`. 

One useful manipulation is to load the argument to the top of the stack. 

        -- loadkw :: ("kw" * env) ~> (x * (env-("kw",x)))
        loadkw =| matchkw
        loadkw =| rot2 [loadkw] tail rot2

        -- matchkw :: ("kw" * (("kw" * x) * env) ~> (x * env)
        matchkw = assoclp [assoclp assertEq first] first 

        -- assertEq :: (Static x * Static x) * y ~> y 
        assertEq = [eq] first assertStatic

        assertStatic :: (Static Boolean * x) ~> x -- prim
        eqStaticString :: (Static String * Static String) ~> Static Boolean -- prim
        eq =| eqStaticString -- eq can compare static strings
        eq =| eqStaticInt -- eq can compare lots of things

The only new trick here is the primitive assertStatic, which enables developers to perform an ad-hoc compile-time checks on static values. If the check fails (e.g. when the wrong keyword is compared by matchkw) it is treated the same as a type error, and loadkw will be forced to search further. 

The above implementation is non-deterministic if there happens to more than one element using a keyword. This is important: `=|` does NOT imply any ordering. This isn't necessarily a problem: if the same keyword is used for signals of different types, the decision will be disambiguated downstream. Nonetheless, I imagine many developers would feel *uncomfortable* not knowing precisely which element is selected. 

Fortunately, this can be solved easily. We can modify the second loadkw meaning to assert the first `("keyword",argument)` pair is NOT a match before looking further. Basically, that gives us traditional name shadowing. (Left as an exercise for the reader.)

### PICK and ROLL

In FORTH, common stack operations are copying an element to the top, or moving it to the top. Rather than writing out a different word for every depth, we might choose to abstract the depth argument into a parameter. In FORTH, this results in the operations PICK and ROLL. In most use cases, the depth argument is static. Awelon can model pick and roll with static arguments.

        -- ifEq0 :: (Static Int * x) ~> x
        -- validate value = 0, remove it
        if0 = [0] first assertEq
  
        -- ifN-- :: (Static Int * x) ~> (Static Int * x)
        -- validate value > 0, decrement it
        ifN-- = [copy 0 assertGreater decrement] first

        -- pick will copy the Kth element to the top of the stack. 
        pick =| if0 first assertEq copy
        pick =| ifN-- rot2 [pick] tail rot2

        -- roll will rotate the Kth element to the top of the stack, removing it.
        roll =| if0 -- done
        roll =| ifN-- rot2 [roll] tail rot2

### Sloppy Programming

Keywords, pick, roll, dup, head, tail, etc. are all examples of *precise* programming - they work well when developers know exactly what they want. 

However, there is also a valuable role in this wide world for *sloppy* programming. Use cases include rapid prototyping, plug-and-pray integration efforts, auto-configuration against available features, and live programming environments where active developers can easily gain feedback and refine on-the-fly.

Awelon's overloading supports sloppy programming. The use of `=|` is non-deterministic at compile time. It's easy to get sloppy with non-determinism. As noted with the keywords example, it actually takes more effort to avoid it.

However, non-determinism (by itself) is terribly unsatisfying as a sloppy programming model. 

In most cases, developers have murky intuitions, a rough grasp of which options they favor, and which are just defaults or fallbacks. Developers, potentially more than one, are tossing features together and thinking: "hey, it would be really cool if you can use this" and "here's an awesome idea!" and "oh, this is an interesting possibility". It is important that developers be able to *express* these notions to the computer, such that the computer can prioritize whichever solution the developer currently imagines he prefers.

Paradigms developed for sloppy programming typically include *weighted* logics and soft constraint systems (cf. [dyna](http://www.dyna.org/)). Essentially, a scoring mechanism is applied to the different solutions, and the implementation searches for solutions having high scores. Depending on the programming environment and whether a human is in the loop, developers might have opportunity to view multiple solutions (or partial solutions) and further clarify or select between them. (Finding a global optimum isn't essential because weights provided by humans are hand-wavy estimates of their true feelings.)

Awelon supports scoring by simple use of a few primitives


By nature of intutions and preferences, humans don't have a very precise grasp of them.

ir own intuitions and preferences. 

Humans can't distinguish their preferences that precisely. 



, but if those features aren't compatible . But those features aren't necessarily compatible, so developers start thinking ab

 *awesome* and which options are *meh, if you can't do that do this instead*

 with sloppy programming, we have some idea of which options we'd *prefer*, and which are 


adaptive or context-sensitive systems development where we might take advantage of 'optional' features if they're available, and so on.

 type-driven programming was used in a precise manner: to select keywords, to 

First, we need a new primitive and some helper functions that will allow us to test whether we've found the keyword we were seeking:

        staticAssert :: (Static Boolean :&: x) ~> x -- prim

A static assertion allows developers to test static va failures. 



Getting back to the problem of data-plumbing, it is feasible to create definitions that try different ways of fitting program components together. Here's one potential example:

        -- applyInStack :: (Static (x ~> y) * (a * (b * (... x? ...)))) 
        --              ~> (a * (b * (... y ...)))
        applyInStack = head | [applyInStack] tail

However, be warned: the chocie `|` bar is not deterministic! It certainly isn't *random*, but developers may still have difficulty predicting or understanding the choice. Developers cannot even assume a preference for the first option, because there are too many factors involved. Consider: in `[foo] applyInStack` both foo and the apply are searching for meaning... but so is the stack itself, i.e. the type for `a` might depend on choices upstream, and the `y` type might depend on choices downstream. (Awelon supports type-safe composition. It's just, we don't locally know which types we're composing.)

To better fit developer expectations, developers need more influence over the search. Awelon provides a few such mechanisms. 


### Preferences and Soft Constraints

Score both the solution (quality) and the search (progress)

 This can be achieved by both 'hard' and 'soft' mechanisms.

### Concerns about Terminating Computation


, in `[foo] applyInStack`, both foo and the application are subject to search, so you cannot be sure that `foo` doesn't match both `a` and `b` even if they're different. 


As a result, developers really need some greater control over the search, or at least greater intelligence. To support this, developers have a couple features available to them. First is `assertStatic`, and the second is a `scoring

To support this control, Awelon does support a *soft preferences* model. 

To make this plumbing a bit more intelligent, developers have a few options:

1. It is possible to use static values, along with an `assertStatic :: (Static Boolean :&: e) ~> e` primitive. This would allow developers to create a variation of `applyInStack` that is limited to some fixed depth. 

2. Awelon supports *hidden* type attributes, which serve a similar role to Lisp's special variables except they're limited to compile-time operations. 

 which are a bit like Lisp's special variables - except they're static.

2. Awelon has a *scoring system* to support soft preferences. 

* developers can use static 


, but since search may also be occurring on the quoted `Static (x ~> y)` subprogram we cannot count on choosing it immediately. The solution may be *stable*, however; Awelon compilers may even remember which solutions seemed to work on previous runs.

, i.e. there is no strong ordering property. To help control search, developers can use a *scoring* mechanism to assert that some solutions are preferred more or less than others. But

 First,
 it's worth noting that there is no order on that `|` bar. I.e. `applySomewhere` is non-deterministic if there happens to be two or more valid locations it might apply. To help control the type-driven search, developers can 





If more than one meaning is valid, it might further search for a meaning that 'scores well'. Scores are modeled with simple behaviors:

   



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


Modules System
--------------

I have mixed feelings about supporting a module system in Awelon code. Code using external modules cannot easily be distributed because first the remote system must obtain those modules, and even then it must have the same versions. Also, module systems are not the only way to modularize code, e.g. there are also multi-agent systems, collaborating in a shared space. (We can potentially [model link-time using active software agents](http://awelonblue.wordpress.com/2013/04/16/direct-manipulation-of-the-link-time-environment/).) But, without a traditional module system, I foresee only one outcome: developers copying and pasting code. 

So I've decided to include a module system, despite my mixed feelings.


An externally linked modules system is not essential for separate compilation and modularity. An alternative is to model modules as software agents that independently publish and import requirements.

 Reactive Demand Programming is highly suitable for multi-agent  suitable for developing blackboard systems, Blackboard syste

Multi-agent systems are always *modular* in the sense that we can always add new agents to the system.

Rather, it should be considered a kind of preprocessor for Awelon for local reuse purposes. Even in that role, a modules system is not ideal: rather than relying on an *external linker*, which is not very programmable, it may be wiser to [model the linking process as a staged program](http://awelonblue.wordpress.com/2013/04/16/direct-manipulation-of-the-link-time-environment/), i.e. such that modules are software agents publishing resources and searching for them in a shared environment. The latter option is much more *extensible*, and also inherently *reactive* to live programming.

With this in mind, Awelon's module system is simplistic: developers can *import* a module by name (`import ModuleName;`), optionally using a qualifier (`import ModuleName as M;`). Names within that module then become accessible as either 





As a distribution language, modules should not be supported in Awelon. (I.e. if you send a block of code over to a remote system, it cannot be expected to 'import' anything.) However, modules are certainly convenient for 




As a distribution language, Awelon does not support external link dependencies. That is, there is no means for Awelon code to import definitions from another file. Linking can be achieved either by modeling the link-time environment (e.g. search for resources, or explicit downloads) or by use of a higher level language.

i.e. there is no way for one Awelon object to use definitions from another except copy-and-paste. (That said, an Awelon application could actively observe its environment for the resources it needs. This is perhaps even superior, as it offers great programmatic control over linking and should be highly reactive to resources being installed or removed.)


Despite being a low level language, Awelon does have a rich type system, and is not terrible for human programming. Awelon code is tacit: juxtaposition is composition. Literals, numbers, and quoted code all have a behavioral meaning. The set of available signals effectively forms a stack-like environment (i.e. `(x & (y & (z & ...)))`), which can be accessed using various data plumbing behaviors. Awelon does support local definitions.




Where a higher level language should win is intelligent glue code, auto-wiring, type-driven overloading, module systems. )

Static signals can serve a similar role to parameters. For example:

        bfirst :: b x x' -> b (x :&: y) (x' :&: y) -- from Sirea
            -- becomes static apply left (staple)
        staple :: b (Static (b x y) :&: (x :&: z)) (y :&: z) 

Then the static behavior (`Static (b x y)`) can be introduced

 

Awelon code is designed for tacit programming. Juxtaposition is composition. Literals are simple behaviors that extend their environment. 


. Awelon has a few features orthogonal to its RDP elements, primarily achieved through the type system. 

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




Awelon Objects
==============
 

http://en.wikipedia.org/wiki/Tacit_programming

 (e.g. type safety, linear types, dependent types, [negative and fractional types](http://www.cs.indiana.edu/~rpjames/papers/rational.subc.pdf), tacit programming). 


RDP does not need traditional loops; recursion patterns are better addressed by 'foreach' operations 






Structure of AO
===============

An AO is a relatively compact, finite string of unicode characters in the style of J or APL. White space is ignored. It is not very human readable, rather is intended to be easily transmitted or processed by machines. An IDE may be able to display the AO code in a graphical manner for useful editing or debugging.

The basic concept of RDP is that we process time-varying signals.



The AVM associated with an AO string is generally implicit when the string is transmitted. However, it would be wise to specify the AVM version using a dedicated behavior (defined in the the AVM). 


An AO is represented in a unicode string or UTF-8 file (extension **.ao**). As a unit of distribution, an AO must be pretty much self-contained. That is, there are no implicit link-time dependencies because the recipient might not have access to them. (However, there may be more explicit forms of linking that can be encoded in the RDP behavior.)

i.e. having no link dependencies.  

(*) Awelon does not provide any default mechanism for linking. However, a particular AVM might provide behaviors suitable for compile-time or runtime linking.

The structure of an AO is very simple: a reference to a parent resource (described below), a set of local definitions, and the exported main behavior. The local definitions are with simple substitution into the main behavior.





A single AO represents a *set* of possible RDP behaviors, from which one behavior will be selected based on static type. 

 formally modeled as a tactic for selecting one behavior of that set. The actual behavior associated with an AO 

a *set* of possible RDP behaviors, from which one behavior will be selected based on static type. (That is, an AO is not a library of behaviors; it is just one RDP behavior that happens to be adaptive to its usage context.)

 is desired (for both input and output). 

 based on usage context 

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

Awelon is aimed towards real-time applications and cyber-physical systems - user interfaces, multimedia, sensor networks, robotics, control systems. While any application may be expressed in Awelon, tasks that take an unbounded amount of time must often be modeled as incremental processing of intermediate state, which may be awkward (though does have advantages wrgt fairness, persistence, and process control).

Due to syntactic abstraction, Awelon doesn't have a fixed front-end syntax. Instead, awelon is primarily defined by a typed intermediate application language (intended for distribution), a machine description language (i.e. a set of typed resources and primitives for the target app), and a common module system. See the /doc directory for more information.

Getting Started
===============

Examples
--------






Awelon Tactics
==============

Awelon Tactics is an experimental variation on Awelon. There is no syntactic difference between Awelon Tactics and Awelon, however a couple restrictions are lifted:

* Awelon Tactics may define a word multiple times
* Awelon Tactics may have cyclic definitions

Since a word can have multiple meanings, it may also have an ambiguous type. Instead of specifying a single behavior, an Awelon Tactics object specifies a *set* of behaviors. From this set, one behavior is chosen - one that makes the whole program typecheck. Definitions may be cyclic to account for recursively constructing a program, though there should be at least one case that doesn't result in a cycle (all valid programs are finite).

A potential advantage of an Awelon Tactics language, however, is that I could have a fully type-driven model e.g. to implement `dup` in general:

        dup1 :: b (S p x) (S p x & S p x)
        dup = dup1 | [dup1] apl [dup1] apr  
        dup = dup1 | dup2 


An Awelon Tactics Object (ATO) should be much more adaptive, generic, robust to external changes, reusable in more contexts than a typical AO. If the different meanings are consistent in some way, the ATO can also represent higher level concepts. Also, much glue code can be automated. But there is a price: non-deterministic meanings (and consequently, many meanings that are never discovered or debugged), and a search that (if used for dynamic behaviors) could easily undermine any real-time systems properties.

Awelon Tactics can use both AOs or ATOs as modules. 

I am interested in learning whether ATOs are easier or more difficult than AOs for developers to work with, or perhaps whether they are useful in different domains, or whether some ad-hoc mix might work well.


