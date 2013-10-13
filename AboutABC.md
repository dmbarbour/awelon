# Awelon Bytecode

Awelon Bytecode (ABC) is a primary component of the Awelon project. ABC is a streamable, securable, type-safe, tacit concatenative bytecode. Breaking this down:

* **streamable** Code can be processed in atomic chunks without storing the full stream.
* **securable** Effects are via invocation of capabilities. Distribution of authority is securable.
* **type-safe** Types can generally be statically inferred for the environment in ABC.
* **tacit concatenative** Concatenation is composition. Environment is implicit argument.
* **bytecode** ABC is a UTF-8 stream for ad-hoc text, but the basic ops are encoded in ASCII.

ABC is suitable for pure functional, procedural, and reactive programming. ABC is primarily designed for reactive demand programming (RDP). However, the other models are supported depending on interpretation of capability invocations. 

## Type System

ABC has a few basic types: numbers, text, products, sums, and blocks. 

* **numbers** include integers and rationals of arbitrary precision
* **text** is a unicode string of 
* **products** are pairs of values, the basis for structured values
* **sums** are a choice of values, the basis for conditional expression
* **blocks** are strings of ABC code, i.e. first class functions

Products and sums also have their identity types: unit and void. 

The set of basic types can be extended through use of capabilities. For example, a machine may offer high performance matrix representations, potentially with floating point or limited precision numbers. However, only basic types can be serialized directly or encapsulated in a block. 

The basic types can be augmented with attributes. 

Every atomic value has spatial-temporal attributes, location and latency, describing when and where the value becomes available. Spatial-temporal attributes are useful for modeling distributed computations, heterogeneous systems, and overlay networks. Blocks may be marked no-copy or no-drop and have latency bounds, which are useful for modeling limited resources, obligations, uniqueness. 

A system is free to analyze for properties other than basic ones, e.g. to enforce that certain arguments are integers within a bounded range.

## Tree Structured, Linear Environment

ABC's environment is tree-structured, represented as a deep product. A tree makes it easy to express composite values, concurrent pipelines, and rich environments.

In tacit concatenative programming languages, operations implicitly take the full environment as an input and produce the updated environment as an output. Blocks and primitive combinators enable application on just part of the environment.

In Awelon project, the tree represents a user environment. This environment can be walked, navigated using Huet zipper patterns. The environment also carries a user-model, with hands to carry things, and an inventory of tools/macros to manipulate the environment. Walking a tree while carrying values provides a very intuitive approach to data plumbing, and (critically) one amenable to both user environments and programming by example.

ABC's environment is also linear. While there may be structure sharing optimizations under-the-hood, logically there is no aliasing or structure sharing. (An implementation may perform some interning and structure sharing under-the-hood.) To copy or drop a value is an explicit operation. No garbage collection is necessary. ABC's primitives are pure functions, but due to linearity they are used similarly to sequential, stateful manipulations.

Aliasing and cyclic data can be modeled indirectly, e.g. using association lists, or via external resources.

## Staged Programming

A special location value is 'static', which describes values that can be computed within the current code fragment, at compile time. (Static values still have latency.) Numbers, text, and blocks represented directly in code are always static. Static values can be used at any location we can communicate code.

The notion of 'static' values and computations becomes an effective basis for implicit staged programming. Many computations within code can be performed before running the code. Further, statics are useful for modeling user-defined types. 

## Capabilities and Side Effects

In ABC, side-effects (observation or influence outside the environment) must be expressed through capabilities. The essential idea is that a 'capability' is a secure value that tightly couples access to an effect and authority for it. This tight coupling simplifies security issues: by controlling how these special values are distributed to untrusted subprograms or agents, we control their authority. 

ABC represents capabilities as invocations on opaque text. Capabilities implicitly operate on the full environment.

        $ :: (Cap(x->y) * x) -> y

For security, this text is typically a secure-random GUID, encrypted code, or signed code. In Awelon project, capability text also periodically expires and is replaced by new text, ensuring uniform revocation, and resilience against temporary leaks. This is achieved through reactive demand programming; in RDP, granting a capability is continuous, and to revoke a capability one simply stops granting it.

Within an application, capabilities are generally encapsulated in blocks rather than distributed as text. Further, Awelon project tends to centralize access to capabilities to a 'powerblock' - a block that is queried to access capabilities. This centralization makes it easier both to manage coarse security policies and override/audit deep behaviors of untrusted subprograms. 

ASIDE: Insecure text for capabilities can feasibly be used to extend ABC, i.e. as a form of foreign function interface (FFI). This is not recommended for side-effects, but could be convenient for pure extensions to ABC. For example, if a machine offers high performance matrix or vector operations. NOTE: Awelon project does not use this pattern, instead preferring to represent an FFI by publishing capabilities.

## Uniqueness 

Awelon project depends heavily on modeling unique values. Unique values include:

* **sealer/unsealer pairs** - 
* **exclusive bindings to state**
* **unique IDs** - i.e. GUIDs or URLs

ABC cannot model uniqueness directly, but rather does so using capabilities. 

This is generally achieved by using a linear block as the uniqueness provider. This block can be split, by applying it

## Sealed Values and Encapsulation

A useful set of capabilities is for sealing/unsealing values. A seale
ABC supports sealed values, which models encryption, private encapsulation, and can enforce parametricity. For many programs, dependent types can be inferred.

ABC does not have nominative types, because those are unsuitable for open distributed systems. But users can model new types with static values, and can model ADTs via sealed values.


## Compilation, Parallelization, and Performance

For performance, ABC is typically compiled to native code.

Of course, compilation could be difficult in a streaming bytecode scenario. ABC addresses this concern in a few ways. Streams are processed in atomic chunks (separated by `;`). Each chunk may be compiled. Pattern recognition and memoization can enable reuse of compiled code across chunks. Further, relatively large, commonly used subprograms may be named and accessed by secure hash (via capability). The last provides a unit for compilation and supports efficient serialization. 

In non-streaming scenarios, compilation to native code is easier. For RDP, ABC is designed for staged computation, tracking static values vs. runtime values (via spatial-temporal types). ABC's tacit concatenative structure also simplifies many tasks useful for compilation and optimization, such as pattern recognition, rewriting. 

Direct interpretation of ABC won't be the most efficient. As mentioned before



Direct interpretation can also be efficient enough in some cases.

, compilation can be based either on pattern recognition


## Memory Optimizations

Related to the goal of computational performance is to optimize storage costs for code and large environments. This can be achieved by several stratagems:

* **laziness** parts of the environment can be factored into functional code that generates the environment. This is similar to how serialization works for ABC, and enables refactoring on code to be a basis for compression.
* **structure sharing** when a structure or subprogram is used many times, it can stored once then referenced many times in memory. This also goes for referencing subprograms in a stream.
* **hash naming of structures** if we use secure hashes, we can potentially hash code that constructs a value. This could be modeled using capabilities, or as a dedicated feature for streaming bytecode. 

If an environment contains a lot of repeated structure, it is often possible to compress this

 by 'interning' patterns where common values are identified and pointed to the same locations in memory. Due to ABC's linearity, this must be accompanied by 

it can be valuable to optimize for this.

**Macro Extensible Bytecode?**

A feature I find interesting is to support 'definitions' of bytecodes. For example, let's say that BMP plane 16 is for user-defined bytecodes. Each bytecode can be defined with a small string of ABC or other bytecodes. A bytecode from this plane is simply expanded where it is used.

This feature would significantly simplify compression. And it could be made easy to express:

        : (Number * (Block * x)) -> x 
        [code]#12345:
        (defines unicode #12345 to be [code])
        
A valid concern would be comprehending the 'scope' of a definition. This is part of the stream, not really of the environment. Further, we often compose streams for operating on different parts of the environment (expand X into Y) which could be problematic for definitions, introducing issues of mutability for definitions.

Thought: this feature should be a property of serialization, not of 



 It isn't really part of the environment, it is part of the stream of bytecode that operates on the environment, or even part of the serializer. Further, this feature hinders local analysis and rewriting of the 

I think this might be a BAD IDEA to build into the bytecode. It might make a good feature 

 and otherwise not affect the 




##

An advantage of tacit, concatenative code is that it greatly simplifies some tasks such as pattern recognition, rewriting, refactoring, and compilation. ABC can be directly interpreted - an efficient option for small, ad-hoc chunks of code. But it may be more efficient to compile and optimize large chunks of ABC code that see reuse. 

To support compilation in a distributed or streaming environment

To support efficient serialization, large chunks of ABC code are sometimes 

 

While ABC can be directly interpreted, this will often be inefficient. Compiling chunks of ABC code that appear often

 to be compiled wholly or partially.


ABC is very minimal, eschewing even symmetry

ABC is also 'visible' if not legible. The non-printing control characters are not given meaning. Whitespace (space, newline, return, tab) mean identity, which enables them to be removed. 

designed for systems using [capability-based security](http://en.wikipedia.org/wiki/Capability-based_security). ABC assumes very little [ambient authority](http://en.wikipedia.org/wiki/Ambient_authority), and is essentially pure functional other than capability invocations. Convention leverages this; even FFI is provided as a capability.


* **visible** ABC is encoded in UTF-8 printing characters. Whitspaces and newlines map to the identity operation, enabling ABC code to be formatted to fit fixed-width screens. ABC does not promise legibility, only visibility, but it may become readable with enough exposure.


Unlike traditional bytecodes, ABC does not use sectioned files. 



