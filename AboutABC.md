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

ABC systems should track which values can be computed at compile time, within the current code fragment. These are 'static' values. Numbers, text, and blocks represented directly in code are always static. The notion of 'static' values and computations becomes an effective basis for implicit staged programming. Many computations within code can be performed before running the code. Further, statics are useful for modeling user-defined types. 

## Capabilities and Side Effects

In ABC, side-effects (observation or influence outside the environment) must be expressed through capabilities. The essential idea is that a 'capability' is a secure value that tightly couples access to an effect and authority for it. This tight coupling simplifies security issues: by controlling how these special values are distributed to untrusted subprograms or agents, we control their authority. 

ABC represents capabilities as invocations on opaque text. Capabilities implicitly operate on the full environment.

        $ :: (Cap(x->y) * x) -> y

For security, this text is typically a secure-random GUID, encrypted code, or signed code. In Awelon project, capability text also periodically expires and is replaced by new text, ensuring uniform revocation, and resilience against temporary leaks. This is achieved through reactive demand programming; in RDP, granting a capability is continuous, and to revoke a capability one simply stops granting it.

Within an application, capabilities are generally encapsulated in blocks rather than distributed as text. Further, Awelon project tends to centralize access to capabilities to a 'powerblock' - a block that is queried to access capabilities. This centralization makes it easier both to manage coarse security policies and override/audit deep behaviors of untrusted subprograms. 

ASIDE: Insecure text for capabilities can feasibly be used to extend ABC, i.e. as a form of foreign function interface (FFI). This is not recommended for side-effects, but could be convenient for pure extensions to ABC. For example, if a machine offers high performance matrix or vector operations. NOTE: Awelon project does not use this pattern, instead preferring to represent an FFI by publishing capabilities.

## Sealed Values and Encapsulation

A useful application of capabilities is to model sealer/unsealer pairs. The idea is that we have a pair of capabilities, one of which can seal values such that only the other can unseal them. Sealed values are opaque. There is a good opportunity here for type-system integration: the identity of the sealer can become part of a sealed value's type.

Sealer/unsealer pairs are useful for modeling identity, encapsulation, first class abstract data types, rights amplification, and a variety of other features. Awelon project makes heavy use of this pattern, even statically (i.e. some values can be sealed statically to model module systems, private data).

New, unique sealers can be constructed given a uniqueness source.

## Shared State Resources

Capabilities can access external state resources. In some cases, those resources are also accessible by other agents and services. A powerblock will generally provide access to some collaborative spaces (perhaps with varying levels of security) to publish or discover values. 

By providing a globally unique value, one can exclusively bind external state resource. This enables users to define or update their own state models, enforce many-to-one or one-to-many patterns, and better comprehend who has access to state. In Awelon project, this pattern is used extensively; while there are some external spaces not controlled by any particular agent, those are primarily for volatile communications, publishing services, discovering resources, and bootstrapping connections.

## Stable Uniqueness Source

Sealer/unsealer pairs, exclusive state, identity values, and other constructs require uniqueness. Further, these unique values should be stable across changes in source code, to be robust in presence of persistence or update. Awelon models a uniqueness source as a no-copy block that encapsulates a unique capability. 

Functions are pure, and RDP behaviors are idempotent; in both cases the same output must be returned for the same input. Thus, to have a formally unique output requires a formally unique input. Uniqueness sources cannot be 'created', only used if they already exist. Uniqueness sources in Awelon project are provided as initial arguments, otherwise they would not exist at all. 

A uniqueness source cannot be copied (or it would no longer be unique). However, it may be partitioned to arbitrary depth. For stability, it is necessary that the partitions be stable to rearranging, adding, or removing most code. In Awelon project, this is achieved using a parent/child metaphor, similar to directories in a filesystem. A parent must give each child a unique name, but the child has access to a fresh set of names.

In Awelon project, uniqueness is distributed in two distinct stages. In the first stage, user actions are modeled as pure functional streaming ABC code that manipulate the environment. In the second stage, the environment's structure is interpreted as an RDP behavior, which may continuously influence and observe the real world. Unique values in first stage enable objects to be moved without losing their identity or unique external bindings. Unique values in second stage enables dynamic state. 

Both stages use the same stability model. This consistency is valuable in case of history rewriting or programming by example.

In the first stage, however, developers can only distribute uniqueness sources. Capabilities to utilize uniqueness don't become available until the second stage. (Thought: It may be necessary to model the transfer from first stage to second stage uniqueness.)

## Compilation, Parallelization, and Performance

The intention is that ABC is compiled to native code, or to an intermediate code (e.g. LLVM) 

For performance, ABC is typically compiled to native code.



Of course, compilation could be difficult in a streaming bytecode scenario. ABC addresses this concern in a few ways. Streams are processed in atomic chunks (separated by `;`). Each chunk may be compiled. Pattern recognition and memoization can enable reuse of compiled code across chunks. Further, relatively large, commonly used subprograms may be named and accessed by secure hash (via capability). The last provides a unit for compilation and supports efficient serialization. 

In non-streaming scenarios, compilation to native code is easier. For RDP, ABC is designed for staged computation, tracking static values vs. runtime values (via spatial-temporal types). ABC's tacit concatenative structure also simplifies many tasks useful for compilation and optimization, such as pattern recognition, rewriting. 

Direct interpretation of ABC won't be the most efficient. As mentioned before



Direct interpretation can also be efficient enough in some cases.

, compilation can be based either on pattern recognition


## Storage Models

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



