# Awelon Bytecode

Awelon Bytecode (ABC) is a primary component of the Awelon project. ABC is a streamable, securable, type-safe, tacit concatenative, functional bytecode. Breaking this down:

* **streamable** supports well-behaved incremental processing
* **securable** Effects are via invocation of capabilities. Distribution of authority is securable.
* **typesafe** Types can generally be statically inferred for the environment in ABC.
* **tacit** ABC does not use names; environment is implicit argument to every code.
* **concatenative** juxtaposition is composition; to addend is to edit
* **functional** higher order via blocks; immutable tree-structured environment; pure up to capability invocation
* **bytecode** ABC is a UTF-8 stream for ad-hoc text, but the basic ops are encoded in ASCII.

ABC is suitable for functional, procedural, and reactive programming. ABC is primarily designed for reactive demand programming (RDP). However, the other models are supported depending on interpretation of capability invocations. 

## The ABC Stream

ABC is represented in a stream of UTF-8 encoded characters. There are no sections, no headers or footers. There is just the stream, potentially unbounded in length. 

ABC is designed to support casual inspection by a human reading a stream as text, thus all codes are visible and printable. ABC has special support for encoding text, capabilities, and blocks. Further, the encoding of natural numbers in bytecode is very readable. 
        
        [blocks[nestable]]
        #42
        "text begins with a double-quote, and has a block-like format.
         text may extend multiple lines, each continued by LF SP (10 32)
         in which case the LF becomes part of the text. Otherwise, text 
         can be terminated by LF-`~` (10 126), which adds no characters.
         ABC text has no need for escapes, other than for LF.
        ~
        {capabilityText}

ABC also assigns the meaning 'identity' to characters LF (10) and SP (32) to support simple formatting, e.g. to fit ABC to a text display. And other ABC codes are in the Latin-1 set. ABC isn't intended for direct human use, but I believe the effort to keep ABC relatively readable will be valuable for anyone who wishes to peek under the hood - e.g. for learning or debugging.

### ABC Paragraphs

ABC encourages an informal notion of "paragraphs". A paragraph separates a batch of code, serving as a soft, discretionary indicator that "this is a good place for incremental processing". A well-behaved ABC stream should provide relatively small paragraphs (up to a few kilobytes), and a well-behaved ABC stream processor should process a whole number of paragraphs in each step.

A paragraph is expressed by simply including a blank line within code. I.e. LF LF in the stream. 

Paragraphs may be enforced in layers wrapping ABC. For example, a networked ABC-based service may limit paragraph size - and simply disconnect from a client that behaves poorly (perhaps with soft, probabilistic tolerance). Enforced paragraphs are convenient as an implicit boundary for batching, typechecking, compilation, atomic update, while still enabling flexibility to process multiple batches together (e.g. for performance reasons)

Formally, the space between paragraphs just means identity. Paragraphs are not preserved within blocks, and preserving them is discretionary within the toplevel stream.

## ABC Behavior Overview

The ABC stream transforms a value within an implicit environment. 

The basic structural value of ABC is a pair. Pairs are used idiomatically to model stacks, trees, association lists, and other complex data structures, and are ultimately used to model the computation environment. Each primitive operator manipulates only a small part of the value near the root of the value structure, returning the rest unobserved and untouched.

Besides the pair, ABC supports numbers, text, and blocks. Numbers and text aren't anything special. Blocks are ABC's equivalent to first-class functions; they enable higher order programming, loops, and compression. 

### Effects and Capabilities

Primitive ABC operators are purely functional transforms. However, ABC supports effects by invoking environment-defined operators. This invocation is expressed as `{foo}`, accepting the tacit value as the input.

The text 'foo' is called the *capability text*. In practice, this text should be cryptographically secure, potentially specific to the runtime instance. Secure random GUIDs, encrypted text, or signed text (HMAC, PKI) are all possibilities. Capability text is unforgeable from within ABC: there are no primitives that can invoke text as capability text. 

ABC is designed for [capability-based security](http://en.wikipedia.org/wiki/Capability-based_security). Capability-based security conflates controlling access with controlling distribution of unforgeable values called "capabilities". A capability can be modeled as a block containing a capability invocation, i.e. `[{foo}]`. Blocks are opaque in ABC: there are no primitives that can compare blocks or convert a block to text.

#### Distribution of Capabilities

When ABC code represents an application, capabilities should be distributed through the application as part of the computation. Awelon project uses the concept of a 'powerblock' - a central block that can be queried for capabilities - as the primary distribution mechanism. The powerblock can include logic, model security policies and preferences.

As a general rule, capability text should exist only in "live" ABC streams, not in persistent code. Controlling the distribution of capabilities within the computation results in applications that are much more extensible, debuggable, and reusable. Also, in RDP systems, capability text occasionally expires and is replaced reactively (modeling RDP's logically continuous expiration).

### Data is Code

Modeling data with code offers powerful features for compression, validation, procedural generation. A pure, tacit concatenative code like ABC makes this even more convenient and powerful: the code can be streamed, the stream represents a history, the history can be introspected, rewritten, undone. Lenses and views can be modeled with simple compositions. Dead-code analysis for a particular view is much more uniform and straightforward. 

ABC's philosophy is that structured data should be expressed as a linear sequence of code that constructs it. Streaming code can be used for serializing data, and for keeping data up-to-date after serialization. An ABC file can model rich structure, and addending that file will model editing of the structure.

RDP, for which ABC was developed, has a similar philosophy albeit integrating runtime observations and influence. In RDP, structured data is often a live, reactive 'view' of data.

### Types and Type Attributes

ABC's basic type system isn't anything special - numbers, text, blocks, pairs. ABC is *structurally* typed, in the sense that type isn't 

However, ABC richly augments these types with attributes:

* location and latency properties model where and when values can be accessed
* sealed value types model information-hiding and rights amplification
* substructural types for blocks model obligations and resource limitations
* temporal constraints for blocks - use-before, use-no-earlier-than
* track potential ranges for numbers, e.g. protect against divide-by-zero
* (maybe) hints on blocks for lazy or parallel evaluation
* (maybe) enforcable purity property for blocks

Attributes can express and enforce some very useful properties. Note: moving values (between locations) and sealing values are both capability based. ABC type systems assume a conventional class of capabilities that enable these features.

## ABC Behavior Details

This section has a more detailed description of the ABC behaviors.

Note that most ABC operators have a `* e` term on the right, indicating that the operation ignores a certain amount of structure. The goal here is to avoid using blocks for every little thing.

### Data Plumbing

ABC provides a minimal set of primitive operators for block-free structure manipulation. Below, `(a * b)` denotes a product (pair), and `1` denotes unit type (only one value).

        l :: (a * (b * c)) -> ((a * b) * c)
        r :: ((a * b) * c) -> (a * (b * c))
        w :: (a * (b * c)) -> (b * (a * c))
        z :: ((a * b) * (c * d)) -> ((a * c) * (b * d))
        v :: a -> (a * 1)
        c :: (a * 1) -> a

There are many potential "minimal sets" of data plumbing primitives. ABC's particular set is aiming for some simple symmetries and optimizations. Here, `lzrw` are sufficient to encode any linear manipulations of a structure where the rightmost element remains in place. `v` and `c` are non-linear manipulations (they add and remove structure) and enable motion of the rightmost element. 

Example encodings:
        
        lzrw :: (a * (b * (c * d))) -> (c * (a * (b * d))) -- rot3
        vrwlc :: (a * b) -> (b * a) -- swap

Data plumbing code is often the bulk of the ABC stream, and can be built on metaphors like stack manipulators, navigation, and search. A good compiler should greatly optimize much data plumbing code from the runtime.

In addition to moving objects around, we can potentially drop or copy values:

        % :: (Droppable x) => (x * e) -> e
        ^ :: (Copyable x) => (x * e) -> x * (x * e)

ABC does not assume all values can be dropped or copied. A block can be marked affine (no copy) or relevant (no drop), and a block with both properties can be called linear. Expression of these substructural properties is useful for enforcing structured behavior in the absence of structured syntax - e.g. completion of a protocol or handshake. 

Text, numbers, and the unit value may always be dropped and copied. A product or sum can be copied or dropped if all elements have the same feature.

### Blocks

A block in ABC is simply a container for a finite sequence of ABC code. 

ABC supports block literals by use of square brackets. In addition to literal construction, blocks may be composed with the `o` operator, and blocks can be formed from many values by use of the `'` (single quote) quotation operator. 

        [vrwlc] :: e -> ([(x * y) -> (y * x)] * e)
        o :: [y->z] * ([x->y] * e) -> ([x->z] * e)
            [abc][def]o = [abcdef]
        ' :: (Quotable x) => x * e -> [1->x] * e
            #42' = [#42c]
            [abc]' = [[abc]c]

Not every value type is quotable. Blocks, numbers, and text are quotable. Products and sums are sometimes quotable, but only when both elements are quotable and have the same location and latency attributes. 

After construction, a block can be applied by the `$` operator.

        $ :: [x->x'] * (x * e) -> (x' * e)

Blocks in ABC form a basis for secure reasoning, loop behaviors, and higher order programming. 

Blocks offer two security properties. First, note how `$` applies a block to only `x` in `(x * e)`. Use of blocks makes it easy to control how much of the environment a distrusted subprogram can observe or influence. Second, blocks encapsulate information, behavior, and authority. There are no primitive operators that can introspect a block, nor any to forge capability text that might exist within a block.

Of course, developers may express inline application of a block by `v$c`. And there may be capabilities to support introspection of a block.

Loops are modeled by behaviors that repeatedly copy and apply a block. The trivial "forever loop" of Lambda calculus `(\x.(x x) \x.(x x))` might be expressed in ABC as `[^$]^$`. Of course, useful loops should have escape condition (see conditional behavior, below). Note that ABC does not use any form of recursion.

Higher order behaviors are modeled by separating the definition of the block from its site of application. Capability secure programming is often modeled this way: effects are achieved through blocks provided as arguments. 

### Numbers

ABC's built in number type is arbitrary precision rationals. ABC systems might support a vector or matrix of floating point numbers via capabilities, but floating point is not a standard part of ABC.

ABC has no number literals. `#42` is technically a sequence of three ABC operators. The operator `#` introduces the number 0 into the environment, and each decimal digit (0-9) means "multiply by ten and add this digit's value". However, the end result is that natural numbers are expressed as if by literal, but without any special reader state. 

        # :: e -> N(0) * e
        0 :: N(x) * e -> N(10x+0) * e
        1 :: N(x) * e -> N(10x+1) * e
        2 :: N(x) * e -> N(10x+2) * e
        ...
        9 :: N(x) * e -> N(10x+9) * e

Rationals must be represented as a computation that generates them. ABC provides only a few elementary mathematical operators: add, multiply, and additive or multiplicative inverses.

        + :: (N(a) * (N(b) * e)) -> (N(a+b) * e)
        * :: (N(a) * (N(b) * e)) -> (N(a*b) * e)
        - :: (N(a) * e) -> (N(0-a) * e)
        / :: (N(non-zero a) * e) -> (N(1/a) * e)
            type error if a is possibly zero

A few example numbers might be:

        #2#3/*-  (-2/3)
        #123/00/ (1.23)

Languages built above ABC, such as AO, will generally provide a more traditional and compact syntax for numbers. 

ABC is not rich in math. ABC has just enough to easily express rational numbers and latencies (albeit, not in a particularly compact manner), but ABC doesn't get much beyond elementary school mathematics. To model a square root or trigonometric function will likely require computing a series with tolerances, or modeling irrational numbers as codata. 

Rather than relying on a smart compiler to recognize and optimize math, it seems wise to model rich mathematical expressions as DSLs - declarative sets of equations that can be simplified, analyzed, rewritten, interpreted in ABC if necessary, and more explicitly compiled to CUDA or another external language when simulation is necessary. 

These programmatic representations of numbers are perhaps not optimal syntactically, but they do keep numbers visible for casual perusal of the ABC source, and they also keep the parser of ABC very simple. The AO language, which is a thin layer above ABC, provides more traditional syntax for numbers.


Other comp

ABC does

        #42         :: e -> (Number * e)

### Text

        "text...    :: e -> (Text * e)

### Conditional Behavior

An equivalent set of data plumbing operators exists for the sum type, using the same set of characters but capitalized (`LRWZVX`). Sum types are discussed later, regarding conditional expressions.

### Parallelism and Laziness (tentative)



### Substructural Reasoning

ABC has first-class support for substructural typed blocks have special attributes in ABC to support sub-structural reasoning. A block can be marked affine (no copy) and relevant (no drop), or both - in which case the block is called linear.

        f :: (Block * e) -> (Block' * e) (attrib affine, no copy)
        v :: (Block * e) -> (Block' * e) (attrib relevant, no drop)


When two blocks are composed, the composite inherits the substructural properties. Similarly, if a structure containing a block is quoted, the quotation inherits the substructural properties. 

Introducing an attribute is idempotent. When two blocks are composed, the composite has the affine and relevant attributes from both components. For example, if we compose an affine block with a relevant block, the result is a linear block.

Affine and relevant blocks are useful for modeling resources, obligations, uniqueness, exclusivity, and generally for achieving structural programming guarantees without the structure.

## Considerations and Conventions for Implementations

### Structure Sharing for Serialization and Storage

For sequences of ABC that see a great deal of reuse, it can be worthwhile to name the sequence and access it by name. Doing so can optimize serialization and better support reuse of compilation efforts. ABC doesn't have any built-in support for naming, but capabilities and conventions can be combined to similar effect. 

        {#secureHashOfCodeInBase64}

The meaning of this code is quite simple: inline expansion of the referenced code. The referenced code may contain more such references. This behavior falls within the normal semantics of capability invocations. It is suggested that most ABC environments provide support for this feature by default. 

Presumably, one could use URLs or some other model of names than a secure hash. However, URLs would introduce complications involving cyclic definitions, update and cache invalidation, duplication across services, and so on. Secure hashes provide globally unique names, prevent cycles, separate the update concern, and enable flexible replication, storage, and content-based lookup processes. Hashes also enable validation. *Use of a secure hash is simply better design.*

This technique can be augmented further with cloud storage or p2p distribution. In these cases, we may wish to prevent peeping from the distribution service, which suggests use of encryption. To standardize this convention, the following format is suggested:

        {#secureHash:secureRandomEncryptionKey}

The secure hash is computed on the *encrypted form* of the code. Lookup uses only the secure hash, not the key. A fresh secure-random key is chosen for every encoded object. The key is always distributed in the above format, tightly coupled to the identifier. Some random junk may also be added to the code to further protect against attacks based on similar openings, e.g. via constructing a text literal then dropping it. Naturally, when code is distributed in this form, the ABC stream should also be encrypted.

I expect this technique to be pervasively used in Awelon project:

* feature enabled by default across ABC installations
*   built into serialization code at least for reading
*   disabled where necessary, which shouldn't be often
* prefix `#` is assumed by convention to have this meaning
* SHA2-256 for secure hash, with two tag/pad bits at front
*   00 - indicates use of SHA2-256 hash
*   other tags reserved for potential upgrade to SHA3 or similar
* infix `:` indicates encryption with given ke
* Again, have two tag bits:
*   00 - indicates AES with 192-bit key (including the 00)
*   other tags reserved for upgrade in case AES compromised
* Encode is base64url (`A-Z`,`a-z`,`0-9`,`-_`) no space, no pad
*   46 octets including curly braces and `#` 
*   79 octets for cloud storage encrypted code

This technique can be used both for streaming code (a basis for caching, optimized serialization, useful boundary for compiled code) and for storage (as a basis for structure-sharing compression, interning, abstraction). In a stream, one can generally ask for a value whenever it is necessary, so we can use ad-hoc cleanup mechanisms when we're tight for space. For storage, tracking dependencies and proper garbage collection would be more essential.

An interesting use of this feature is to gradually - and continuously - refactor and refine a large cross-project codebase to improve compression, reuse, performance, etc.. Services might provide features to automatically factor ABC code with a global perspective about which reuse is actually effective.

### Convention: Annotations!

Through capabilities, ABC supports a notion of "annotations". Annotations provide hints to a compiler, optimizer, theorem prover, or debugger. However, annotations must not impact the observable, formal semantics of the program. That is, removing the annotation should not change the meaning of the program.

Example applications for annotations:

* suggest that a block be computed on GPU or FPGA
* suggest lazy or parallel computation of pure function
* suggest a pure block use a memoization table
* suggest specialization/optimization of a block
* tweak heap sizes, quotas, tolerances
* indicate output to a named error logger in debug mode
* indicate which values or volumes a debugger should render
* indicate which colors or styles a debugger should use
* indicate where to print type information at compile time
* help compiler provide better blame and error messages
* provide hints for a typechecker that might be confused
* provide assertions or tactics to a theorem prover

Annotations can be very useful even though they don't have any formal meaning in the program. Obviously, a lot more convention and standardization would be valuable to fill out a common set of annotations. 

ABC systems should support an extensible set of annotations by the simple means of ignoring those they do not recognize. Since annotations should not affect observable behavior, ignoring them is not an error.

### Pointers as Capabilities

In some cases, e.g. when referencing a large matrix of floating point numbers on the GPU, it would be extremely inefficient to represent capabilities as text. 

### Design Constraint: No "Optional" Capabilities

When designing ABC, I was initially tempted to introduce a class of optional capabilities, `{?foo}` whose convention is to return a sum type depending on whether `foo` is recognized as a capability or not. This would make ABC very adaptive to different environments. However, this would have awful properties for configuration management, testing, modeling mockup environments. In retrospect, it's an awful idea. Whether capabilities 'exist' or not should be modeled at the point where they are acquired, not where they are applied.

### Tail Call Elimination and Inlining

ABC is easily implemented using a stack, with each frame on the stack corresponding to a block application (via `$` or `?`). [Tail call](http://en.wikipedia.org/wiki/Tail_call) elimination is not essential unless ABC is used to model processes as looping procedural code. (ABC is intended for use in RDP, where such loops are not used.) But it can be convenient, and it isn't difficult to recognize or implement. For example, a block ends in `$]` or `$c]`, its record on the stack can generally be recovered.

A system should also opportunities for inlining code. Two common opportunities will likely be blocks of the form `[v...c]` or applications of the form `v$c`. 

### Linear Implementations or Garbage Collection?

In ABC, use of drop and copy operations (`%` and `^`) is explicit. ABC can feasibly be implemented in a linear fashion, performing a deep copy whenever a copy action is requested, perhaps optimizing for dead code. But ABC could also be implemented with structure sharing or even interning of values. 

In the latter case, ABC implementations should use some form of garbage collection. Reference counting is quite feasible due to the explicit copy and drop. A compacting collector should also work well. 

### Compilation

ABC can be interpreted. But ABC is designed under the assumption that it will be compiled. In a streaming code scenario, ABC could be just-in-time compiled at paragraph boundaries, up to a few paragraphs at a time. 

A good compiler should:

* perform static type-checking of code
* reduce data plumbing to register and memory access
* allocate memory in volumes or regions, not tiny pairs
* inline blocks where possible
* eliminate obvious dead-code
* perform tail-call elimination
* identify potential volumes for parallel or lazy computations
* identify volumes of code that can be shifted to GPU or FPGA

Even if interpreted, a good interpreter might have optimized implementations for common subsequences, and might optimize simple tail-calls.



### Text







A block contains arbitrary ABC code, and preser

Blocks are val enable higher-order programming: A block contains arbitrary ABC code, but preserve it for use in a different context. Blocks can als

enable higher-order programming. A block simply contains more ABC code. ABC doesn't support recursion, but use of fixpoint combinators can achieve the same purpose.




Then there are many useful primitives for data plumbing, operations on text, etc.

        

        ^           

Blocks enable higher order programming. A block simply contains more ABC code, which may later be invoked to operate on just part of the environment. 

        f

In addition, there are a number of data-plumbing operations:

        f

Capabilities will directly operate as though they were inlined.

        {capability}    :: (



There are also several data plumbing operations:

        f               ::


## Implementation of ABC


 There is no stateful aliasing. However, the environment is effectively linear: no implicit copy of values is kept around, there are no variables to hold onto values. Thus, ABC can be implemented using destructive operators, and generally without garbage collection.


## ABC for Serialization and Storage

ABC is promising as a serialization and storage mechanism for structured data. 

For serialization, ABC can be used both for transmission of structured information and to keep it up-to-date. To help keep the remote value up-to-date, we should use a `(value * workbench)` pair so the serializer can refactor the stream (by building reusable abstractions at the recipient). 

For storage, I envision a **.abcd** (Awelon Bytecode Data) file which contains pure ABC code (no capabilities). One might obtain the value structure by applying a unit input to this file. ABC for storage has a few interesting properties:

* formal factoring and optimization can compress large values
* to addend is to edit; ABC sequence forms an implicit undo log
* support large, procedurally generated structures 
* represent lenses, views; potential for lazy computation


## ABC Environment

 





### Need More Types?

The set of basic types can be extended through use of capabilities. For example, a machine may offer high performance matrix representations, potentially with floating point or limited precision numbers. 

However, only basic types can be serialized directly or encapsulated in a block. In general, to a system that provides alternative types must provide mechanisms to serialize them to ABC code for construction elsewhere.

### Spatial-Temporal Types

These basic types are be augmented with attributes. 

Any number, text, or other point type has spatial-temporal attributes, **location** and **latency**, describing when and where the value becomes available. Spatial-temporal attributes are useful for modeling distributed computations, heterogeneous systems, and overlay networks. Blocks may be marked no-copy or no-drop and have latency bounds, which are useful for modeling limited resources, obligations, uniqueness. 




## Idioms

## Tree Structured Environment

In tacit concatenative programming languages, operations implicitly take the full environment as an input and produce the updated environment as an output. Blocks and primitive combinators enable application on just part of the environment. ABC's environment is tree-structured, represented using the product type. A tree makes it easy to express composite values, concurrent pipelines, and rich environment structure.

Access to the *structure* of the environment greatly empowers convention. ABC enables coding in many different environments, and composing code from different environments whenever the interfaces are relatively narrow. 

In Awelon project, the tree represents a user environment - desktop, VR or AR world, etc.. This environment can be walked, navigated using Huet zipper patterns. The environment also carries a user-model, with hands to carry things - clipboard, inventory, brushes or other tools. Walking a tree while carrying values provides a very intuitive approach to data plumbing, and (critically) one amenable to both user environments and programming by example.

An important point is that there is no aliasing or shared structure within the environment. The Awelon environment is effectively linear, though many structures can be copied. 

I.e. modifying one part of the environment cannot directly affect the structure elsewhere. 

ABC's environment is also linear. While there may be structure sharing optimizations under-the-hood, logically there is no aliasing or structure sharing. (An implementation may perform some interning and structure sharing under-the-hood.) To copy or drop a value is an explicit operation. No garbage collection is necessary. ABC's primitives are pure functions, but due to linearity they are used similarly to sequential, stateful manipulations.

Aliasing and cyclic data can be modeled indirectly, e.g. using association lists, or via external resources.





## Staged Programming

ABC supports explicit staging by use of 'blocks' as output from another function. In some implementations, stable blocks might be compiled into native code prior to execution.

Staging can also be modeled through location types with asymmetric transport. Capabilities might only be available in one partition or another. When modeling spatial-temporal types for values, an implicit type is 'static' which essentially means 'available here and now'. Static computations are computations that take static inputs and produce static outputs. Static computations can generally be performed at compile-time.



ABC supports implicit staging by tracking 'static' values and partial evaluation. Numbers, text, and blocks may be 'static' in the sense that they can be computed without use of runtime inputs. Static numbers and text fragments can be understood as part of the type, useful for modeling records and other structures. Many of these computations can be eliminated by a compiler.



Awelon project makes heavy use of staging. In particular, there is a strong staging between manipulation of the user environment and the interpretation of the user environment to ensure that long-running behaviors are always visible and accessible for update.

## Capabilities and Securable Side Effects

In ABC, side-effects (observation or influence outside the environment) must be expressed through capabilities. A 'capability' is a value that tightly couples access to an effect and authority for it. 

For code executing within a single machine, a capability might have no syntactic representation. However, Awelon project is designed for distributed systems programming, and ABC is designed for streaming. On occasion, it is necessary to map a capability to some secure text. When serialized within ABC, a capability looks like:

        {foocap}

A capability is simply opaque text between `{` and `}`. A capability may contain a balanced set of `{}` pairs, so the capability text extends to the final `}`. 

For security, this 

This opaque text should be a cryptographically secure value. And by 'secure' I mean in the  random GUID, encrypted code, signed code, or similar. If the text is easily learned or computed, then the capability would not provide any security. 

*ASIDE:* use of insecure capabilities as a basis for FFI is feasible, and even seems promising for modeling high-performance pure operations (e.g. vector and matrix operations). However, it is not recommended. Capability distribution patterns are also useful for maintaining, extending, porting, testing, and debugging code. Awelon project favors treating all capabilities as secure.



## Compilation, Parallelization, and Performance

The intention is that ABC is compiled to native code, or to an intermediate code (e.g. LLVM) 

For performance, ABC is typically compiled to native code.

Tail recursion or continuation passing style... behaviors ending in `$`.



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


Older Contents
========================

Many complex values - e.g. lists, trees - can be directly modeled using products. Cyclic graphs can be modeled using adjacency lists or similar. Products and sums don't have any syntactic representation in ABC, but can tacitly be constructed and manipulated. When documenting types, `(a*b)` and `(a+b)` are used for products and sums respectively. 

Sum types in ABC are the basis for conditional expressions. Any decision essentially produces a sum, and conditional operations on that decision can be extended compositionally, instead of if/then/else.

ABC can access additional value types via capabilities or external functions. For example, a machine may support matrices of floating-point numbers and associated math operations. 

## Substructural Types

ABC is designed primarily for RDP, which has several interesting properties for each value:

* 




## Primitives


## Whitespace is Identity

ABC assigns the four common whitespace characters - space, tab, newline, carriage return - the value identity `x -> x`. This enables ABC code to be broken across multiple lines, and represented in neat blocks of plain text. An empty ABC stream also has the meaning identity. I.e. the identity block is just `[]`. 

## Stream Processing of ABC
## Capabilities and External Functions

ABC can invoke external behavior like so: `{foo}$`. The text specifies a capability or function. The dollar operator invokes the function. The input is tacit. Typecheckers and validators should enforce that the text for `$` is static and refers to a known function. A capability computed at runtime should be part of a runtime-provided block, i.e. `[{foo}$]` is essentially a first-class foo capability. 

While transparent words like 'foo' are allowed, they essentially represent an ambient authority and their provider should carefully consider their security implications in an open system. The safest and best use of transparent words is to extend a system with *pure* functions and types - e.g. optimized or GPU-layer vector, matrix, floating point ops, and so on.

Instead of transparent words, developers should use 'capabilities' - cryptographically secure tokens whose origin can be traced. Capabilities might be modeled by encrypted code, code protected by HMAC, or a secure-random GUID associated with local code. In general, the meaning of a capability is known only to its initial provider, opaque to everyone else.

Due to the opaque nature of capabilities in an open system, it can be difficult to typecheck code containing them. To help address this concern, capabilities should generally be bracketed or wrapped with static type-assertions, describing what the input type and output types should be.


## Serialization with ABC

For systems supporting ABC, streaming ABC should be the primary (or exclusive) serialization format. 

For example, rather than sending a pair of numbers `(42,108)`, the output stream might be `{108}#se{42}#` which assumes the initial input is unit. In general, serialization with ABC will be slightly more verbose than a dedicated format for simple, uncompressed values. 


Doing so has several advantages:

* simpler system, only one parser to deal with
* no discontinuity to later introduce functions
* self-validating expansions via assertions, types
* achieve high levels of semantic compression
* reactive systems: update small parts of large values
* can model communication context and private vocab

The last couple points assume a long-running system that either sends many values or keeps one value up-to-date. The idea is that the receiver should provide a dedicated scratch space for the sender, the communication context: `(value * context)`. This context enables the sender to maintain some local values to optimize future updates - e.g. cached or memoized values, a small library of reusable functions to cheaply expand further updates, and so on.

Serialization with code does have a risk. ABC doesn't guarantee termination, but even if it did there would still be a potential denial-of-service risk when executing code. Where this risk is a problem, it should be addressed using deterministic quotas. 

## Compilation Targets?

When compiling, I'd like the ability to target JVM, CLR, JavaScript, LLVM. LLVM might be able to hit the rest.


## MISCELLANEOUS THOUGHTS

Should I provide any support for fixpoint operations? I suppose these are just integral ops.

Design requirements to **support "inlining" of blocks**...

* To inline a block, it must be possible to rearrange the environment then apply every operation in a block to just part of the environment. I.e. capabilities must operate on only part of the environment.
* Thus, never create a code that reads the whole environment. 
* Further, ensure the 'unused' volume has a stable location.


