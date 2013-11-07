# Awelon Bytecode

Awelon Bytecode (ABC) is a primary component of the Awelon project. ABC is a streamable, securable, type-safe, tacit concatenative, causally commutative, spatially idempotent, weakly legible, functional bytecode. Breaking this down:

* **streamable** supports well-behaved incremental processing
* **securable** effects via invocation of unforgeable capability text
* **typesafe** types can enforce many safety properties, and can be inferred
* **tacit** no local variable or parameter names; ops appy to environment
* **concatenative** juxtaposition is composition; to addend is to edit
* **causally commutative** effect ordering is expressed by argument threading
* **spatially idempotent** duplicate expression doesn't duplicate effect
* **weakly legible** visible, formattable code; readable text and numbers
* **functional** higher order expressions, immutable values, pure by default
* **bytecode** UTF-8 for text, but ABC codes within Latin-1 character set

ABC is suitable for functional, procedural, and reactive programming. ABC is primarily designed for reactive demand programming (RDP). ABC can be interpreted, but is intended as an intermediate language to be compiled to native code for execution. 

Programmers generally work in a higher level language that compiles to ABC, such as AO. An interesting property of AO is that, with a good dictionary, both assembly and disassembly of ABC is quite feasible.

Reuse of ABC code is possible and is orthogonal to reuse in the higher level language. Reusable sequences of ABC code may be named by secure hash and referenced via the capability mechanism, and accessed from a local cache or storage. This technique is described in greater detail later.

## The ABC Stream

ABC is represented in a stream of UTF-8 encoded characters. There are no sections, no headers or footers. There is just the stream, potentially unbounded in length. ABC is designed to be visible, printable. Text, blocks, capability invocations, and the encoding of numbers are at least weakly legible:

        [blocks[[are]nestable]]
        #42
        "text begins with a double-quote, and has a block-like format.
         text may extend multiple lines, each continued by LF SP (10 32)
         in which case the LF becomes part of the text. Otherwise, text 
         can be terminated by LF ~ (10 126), which adds no characters.
         ABC text has no need for escapes, other than for LF.
        ~
        {capabilityText}
        vrwlc

This visibility seems useful for didactic purposes and debugging. For similar reasons, ABC supports whitespace (LF (10) and SP (32)) by assigning it the meaning 'identity', to simplify formatting of ABC. 

### ABC Paragraphs

ABC encourages an informal notion of "paragraphs". A paragraph separates a batch of code, serving as a soft, discretionary indicator that "this is a good place for incremental processing". A well-behaved ABC stream should provide relatively small paragraphs (up to a few kilobytes), and a well-behaved ABC stream processor should process a whole number of paragraphs in each step.

A paragraph is expressed by simply including a full, blank line within ABC code. I.e. LF LF in the stream. Much like a paragraph in a text file.

Paragraphs may be enforced in serialization layers - simply disconnect from a client that doesn't provide timely paragraphs of a reasonable size (perhaps with soft, probabilistic tolerance). Paragraphs are convenient as an implicit boundary for batching, typechecking, compilation, and atomic update, while still enabling flexibility to process multiple batches together for performance reasons.

Formally, the space between paragraphs just means identity. Paragraphs are not preserved within blocks, and use of paragraphs is ultimately discretionary within the toplevel stream.

## ABC Behavior Overview

The ABC stream transforms a value within an implicit environment. ABC's basic type system isn't anything special - numbers, blocks, pairs, sums, unit and void. ABC's number type is precise rationals.

The primary structural type of ABC is the pair. Pairs are used idiomatically to model lists, stacks, trees, text, association lists, and other complex data structures. Ultimately, pairs model a complete computation environment. Primitive ABC operations manipulate only a small part of the value near the root of the value structure, returning the rest unobserved and untouched.

ABC relies heavily on structural typing and partial evaluation to understand how a program will behave at runtime. 

### Type Attributes

The basic types are augmented with a rich set of attributes and dependent types:

* location and latency properties model where and when values can be accessed
* sealed value types model information-hiding and rights amplification
* substructural types for blocks model obligations and resource limitations
* latency constraints for blocks and sealed values - expires, ripens
* track potential ranges for numbers, e.g. protect against divide-by-zero

Attributes can express and enforce some very useful properties. Note: moving values (between locations) and sealing values are both capability based. ABC type systems assume a conventional class of capabilities that enable these features.

### Effects

Primitive ABC operators are purely functional transforms. However, ABC supports effects by invoking environment-defined operators. This invocation is expressed as `{foo}`. This calls into the environment with the text 'foo' and the full tacit value. The environment may cause some effect and return an updated value.

These invocations can also be used for annotations, or for referencing external ABC code by secure hash. Such conventions are described much later in the document.

*NOTE:* Nesting, and even appearance thereof, is not allowed in capability text. The character `{` is not allowed within capability text, and the first `}` will end the capability text.

### Capabilities

In `{foo}`, the text 'foo' is called *capability text*. In practice, this text should be cryptographically secure. Secure random GUIDs, encrypted text, or signed text (HMAC, PKI) are all possibilities. In general, this text is also specific to the runtime instance. Capability text is unforgeable from within ABC: there are no primitives that may invoke text as capability text.

ABC is designed for [capability-based security](http://en.wikipedia.org/wiki/Capability-based_security). Capability-based security conflates controlling access with controlling distribution of unforgeable values called "capabilities". A capability can be modeled as a block containing a capability invocation, i.e. `[{foo}]`. Blocks are opaque in ABC: there are no primitives that can compare blocks or convert a block to text.

An environment may, of course, provide introspective and reflective capabilities that can peek inside blocks or translate text to capability text, but such capabilities should be secure and granted only to trusted subprograms.

### Modeling Objects

An ABC behavior might need to work with meshes, buffers, sockets, matrices and vectors of floating point numbers, and other external resources. 

From ABC's perspective, such types must be modeled as blocks that contain highly specific capabilities for objects in the environment. For mutable resources, use of substructural linearity is recommended because it enables typeful control of aliasing, threading, and enforcement of cleanup protocols. Each 'message' sent to an environment objects should typically return a pair consisting of the modified object and any additional results. 

ABC directly deals only with types from its basic set (numbers, blocks, pairs, sums, unit, void). Blocks and capabilities provide the indirection to all other types.

### Causal Commutativity and Spatial Idempotence

ABC universally assumes causal commutativity and spatial idempotence.

Causal commutativity means that there is no ordering relationship unless there is a visible dependency where the output of one behavior is the input of the next. This property is extremely valuable for optimizations (cf. [Causal Commutative Arrows and their Optimization](http://haskell.cs.yale.edu/?post_type=publication&p=72) by Hai Liu, Eric Chang, Paul Hudak). 

        conventional commutativity: (ABC does not have)
            foo bar = bar foo
        causal commutativity: (ABC assumes)
            [foo] first [bar] second = [bar] second [foo] first
              where first  :: [a->a'] * (a*b) -> (a'*b)
                    second :: [b->b'] * (a*(b*c)) -> (a*(b'*c))

Spatial idempotence means that, if the same action is performed twice with the same inputs, there is no additional observable impact. This property is also extremely valuable for optimizations, e.g. in content distribution networks. 

        conventional idempotence: (ABC does not have)
            foo foo = foo
        spatial idempotence: (ABC assumes)
            [foo] first dup = dup [foo] first [foo] second
              where first  :: [a->a'] * (a*b) -> (a'*b)
                    second :: [b->b'] * (a*(b*c)) -> (a*(b'*c))
                    dup    :: (x * e) -> (x * (x * e))

ABC is designed primarily for reactive demand programming (RDP), which has both of these features uniformly and pervasively. ABC enables optimizers to assume these properties even when ABC is not targeting an RDP system. Together, these properties enable (even for effectful code) optimizations and refactorings typically associated with pure functional programming.

Fortunately, these assumptions do not hinder simple expression of safe procedural code. Capabilities can be marked linear, and enforce explicit threading and mutual exclusion. Interestingly, programmers can be much more precise about threading requirements in ABC than they can be in most imperative languages, so a great deal of implicit parallelism, reordering, and synchronization is possible. 

### Fast and Loose Reasoning

ABC favors a philosophy of 'fast and loose reasoning' about termination properties. (cf. [Fast and Loose Reasoning is Morally Correct](http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html), Danielsson, Hughes, et al. POPL 2006.) The idea is that we should reason about programs as if every subprogram terminates. The "assume it terminates" rule applies for loop fusion, parallelization, partial evaluation, equational laws, rewrite optimizations, laziness, and similar. 

Obviously, fast and loose isn't always correct for ABC. ABC is certainly capable of expressing non-terminating programs. The ABC equivalent of lambda-calculus `(λx.(x x) λx.(x x))` is `[^$]^$`. To help encourage fast and loose reasoning, ABC compilers should always perform termination analysis, albeit accepting the possibility that the analysis fails after burning a few cycles:

* if termination proven, compiler is silent
* if non-termination proven, compiler raises error
* if no proof achieved, compiler issues warning

A good compiler would catch obvious non-terminating expressions such as `[^$]^$`. Annotations in code may help a compiler choose appropriate proof strategies, or help isolate warnings.

Between causal commutativity and fast and loose reasoning, ABC should not be considered 'eager' or 'lazy'. The optimizer is given a great deal of flexibility. Programmers may suggest laziness, strictness, or parallelism via annotations, but any such advise is discretionary. Programmers must never *depend* on lazy semantics - e.g. don't use infinite lists, use an explicit stream instead.

Even code with side-effects may be lazy, within the limits of invoking them in a timely manner. However, 'timely manner' means something formal for ABC.

### Well-Timed Behavior

ABC tracks 'logical latency' properties for numbers and blocks. Latency is a rational number, indicating time in seconds from program start. A value may be explicitly delayed, and multiple values may be synchronized (such that they have the same logical delay). Latency constraints can be applied to blocks, enforcing in the type system that they are invoked at valid times.

Logical delay isn't the same as real delay, but a good scheduler should keep them very tightly aligned, with a great deal of predictability. This can be achieved by soft mechanisms, e.g. focusing more resources on computations that are falling behind, speculating ahead to the extent the effects model and buffering allows. It may also be achieved by hard real-time implementations where feasible.

Timing properties for ABC code, whether procedural or RDP based, should be highly predictable. (Related: [Computing Needs Time](http://www.eecs.berkeley.edu/Pubs/TechRpts/2009/EECS-2009-30.pdf), Edward Lee 2009, and [ChucK](http://en.wikipedia.org/wiki/ChucK) - a 'Strongly Timed' language.)

### Incremental Processes

Since ABC code is assumed to terminate, long-running behaviors must be modeled by alternative mechanisms. A few possibilities:

* streaming programs, naturally unbounded
* RDP behaviors, modeling incremental processing


I would recommend use of RDP. But for those who favor procedural models, we could model incremental proc

This is a good thing! The typical `while(true)` loops of 



We don't need non-terminating languages to implement non-terminating systems. RDP systems implicitly continue to operate over time. Streaming programs are naturally unbounded. While ABC truly is a non-terminating language, by the 'fast and loose reasoning' philosophy we are expected to treat it as total.

My recommendation for modeling long-running behavior is to use RDP. ABC is designed for RDP. RDP is a great paradigm, with many nice properties compared to procedural. 

However, ABC is capable of modeling traditional, imperative processes. 

To model long-running imperative processes, the suggestion is to use an explicit process model of a form similar to `type Process a b = a -> (b, Process a b)`, albeit, enhanced with ABC's dependent types, substructural types, causal commutativity, and capability security. This design is considerably more precise, higher order, compositional, optimizable, incremental, concurrent, and controllable than those `while(true)` loops you might have used in procedural languages.

Each step would terminate, but the highest level process would then simply schedule another step, perhaps using the latency properties to precisely guide scheduling and frequency, perhaps taking the current timestamp and some other inputs as functional arguments.

### Data is Code

Structured data in ABC systems is modeled as a stream of ABC code that constructs it. This offers powerful features for compression, validation, procedural generation. Further, it can support history tracking, visualization and animation, ongoing edits as simply addending the stream, and undo or history rewriting. A diff can be modeled by generating a function. 

Optimization of the stream can improve performance or space costs. Lenses and views can be modeled with a simple form of composition. Laziness or dead-code analysis can greatly optimize views.

For connections between ABC systems, data should always be serialized as an ABC stream that self-constructs, rather than a structure that must explicitly be parsed. When ABC is used for RDP systems, the ongoing connection can continue to update the remote structure.

## ABC Behavior Details

This section has a more detailed description of the ABC behaviors.

Note that most ABC operators have a `* e` term on the right, indicating that the operation ignores a certain amount of structure. The goal here is to avoid using blocks for every little thing.

### Data Plumbing

ABC provides a minimal set of primitive operators for block-free structure manipulation. Below, `(a * b)` denotes a product (pair), and `1` denotes the unit type, discussed later.

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

ABC does not assume all values may be dropped or copied. Numbers, text, and unit or void values may be dropped or copied. A product or sum may be copied or dropped if all elements have the same respective feature. Literal blocks can initially be dropped or copied, but may be tagged with substructural properties that forbid drop or copy.

### Blocks

A block in ABC is simply a container for a finite sequence of ABC code. 

ABC supports block literals by use of square brackets. In addition to literal construction, blocks may be composed with the `o` operator, and blocks can be formed from many values by use of the `'` (single quote) quotation operator. 

        [] :: e -> [x->x]*e
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

ABC's built-in number type is arbitrary precision rationals. 

ABC has no number literals. `#42` is technically a sequence of three ABC operators. The operator `#` introduces the number 0 into the environment, and each decimal digit (0-9) means "multiply by ten and add this digit's value".

        # :: e -> N(0) * e
        0 :: N(x) * e -> N(10x+0) * e
        1 :: N(x) * e -> N(10x+1) * e
        2 :: N(x) * e -> N(10x+2) * e
        ...
        9 :: N(x) * e -> N(10x+9) * e

Thus natural numbers are expressed as if by literal, albeit without any special reader state. 

Rationals and negative numbers must be represented as a computation that generates them. ABC provides only a few elementary, scalar mathematical operators: add, multiply, and additive or multiplicative inverses.

        + :: (N(a) * (N(b) * e)) -> (N(a+b) * e)
        * :: (N(a) * (N(b) * e)) -> (N(a*b) * e)
        - :: (N(a) * e) -> (N(0-a) * e)
        / :: (N(non-zero a) * e) -> (N(1/a) * e)
            type error if a is possibly zero

A few example numbers might be:

        #2#3/*-  (-2/3)
        #123/00/ (1.23)

Expression of rational numbers, or very large or small numbers (that call for scientific notation), is not very compact in ABC. A language built above ABC, such as AO, can provide a more traditional and compact syntax for numbers. 

ABC is not rich in math. ABC has just enough to easily express rational numbers, which was motivated for modeling latency types. To model a square root or trigonometric function would likely require iterative computation with tolerances, or modeling irrational numbers as a stream of digits (a block that generates a digit and the next stream).

Anyhow, it seems wise to treat math as a symbolic DSL, especially for rich computations, simplifications. For high performance computing, compiling expressions to OpenCL or an intermediate language could be very effective. Otherwise, the computation could be interpreted.

*BACKGROUND:* Floating point numbers were rejected due to how difficult it is to ensure deterministic semantics and equational reasoning. However, high performance computing environments for ABC should provide, via capabilities, vectors and matrices of floats, and access to GPU computing.

### Text Literals

ABC has a built-in support for representing unicode text as a literal. Literal text is expressed in a block format, encoded in UTF-8 as part of the normal ABC stream:
        
        "text has a block format 
         it starts with double quote
         it may continue on multiple lines
         each ending with linefeed (10)
         and each starting with a space
         terminate with tilde (126)
        ~

By convention, text also starts after a new line, to ensure pretty formatting.

This block structure eliminates need for escapes, except for LF which is escaped by the space. The LF preceding `~` is dropped. If anything other than space or `~` follows LF, the ABC stream is in error. ABC text is capable of quoting ABC code without being too ugly. Sadly, text isn't very aesthetically pleasing for a single word or line:

        "Text
        ~

Text is not a distinct type for ABC. Rather, text is a compact representation for introducing a list of small integers corresponding to the Unicode codepoints. The above code, consisting of six characters, has the same meaning as `#3#116l#120l#101l#84l`, which would have the type: 

        e -> (N(84) * (N(101) * (N(120) * (N(116) * N(3)) ))) * e

The terminal `3` is arbitrary, though has some historical significance as the ETX (end of text) character in ASCII. It also serves as a weak indicator that the list was intended to be interpreted as text. 

*BACKGROUND:* Text was initially envisioned as a distinct type for ABC. However, that design traded a fair amount of simplicity (extra structure manipulation and analysis codes, extra types to track and comprehend, new kinds of code to optimize) for rather dubious performance benefits.

In practice, a streaming ABC interpreter can store text in a compact form, even recognize text when forming tuples. Further, common list-processing operations, such as reverse or addend, could be recognized by simple pattern-matching and swapped for a highly optimized variant. (A compiler, having more time to crunc code, should do even better.)

### Conditional Behavior

TODO

An equivalent set of data plumbing operators exists for the sum type, using the same set of characters but capitalized (`LRWZVX`). Sum types are discussed later, regarding conditional expressions.

### Unit and Void

Unit and void are special types in ABC. Unit (type `1`) is identity for the product type, and Void (type `0`) is identity for the sum type. The principle idea of unit and void is that they introduce structure *without adding information*. I.e. `(a * 1)` has just as much information as `a`, but now it's wrapped in a structure. 

If we aren't careful, we'll have a problem: structure itself can carry information. I.e. `1`, `(1 * 1)`, `(1 * (1 * 1))`, and `((1 * 1) * 1)` are four obviously distinct structures, and distinction can carry information. Fortunately, it's fine that this distinction exists in a meta-layer, so long as it is not observable from *within* the ABC code. 

This feature is achieved in a simple way.

Unit values may not be introspected. If you ask whether `(1 * 1)` is a pair, you'll get an affirmative. However, if you ask whether `1` is a pair, you'll get a type error. Effectively, the condition is observed *outside* ABC, at compile time, and is not observable from within ABC. Interestingly, unit can model vectors, stacks, matrices, etc. where the static size must be known for safe usage.

Void is a logical false, a dead branch. Unless the operations on a void are internally inconsistent, it will typecheck. While we can presumably introspect void, we effectively cannot achieve anything actionable from doing so. Void is potentially useful to enforce that certain conditions *would* be handled by code, even if they are not currently necessary.

### Substructural Reasoning



 These substructural properties are useful for enforcing structured behavior in the absence of structured syntax - e.g. completion of a protocol or handshake. 

ABC has first-class support for substructural typed blocks have special attributes in ABC to support sub-structural reasoning. A block can be marked affine (no copy) and relevant (no drop), or both - in which case the block is called linear.

        k :: (Block * e) -> (Block' * e) (attrib relevant, no drop)
        f :: (Block * e) -> (Block' * e) (attrib affine, no copy)


When two blocks are composed, the composite inherits the substructural properties. Similarly, if a structure containing a block is quoted, the quotation inherits the substructural properties. 

Introducing an attribute is idempotent. When two blocks are composed, the composite has the affine and relevant attributes from both components. For example, if we compose an affine block with a relevant block, the result is a linear block.

Affine and relevant blocks are useful for modeling resources, obligations, uniqueness, exclusivity, and generally for achieving structural programming guarantees without the structure.



## Considerations and Conventions for Implementations

### Structure Sharing for Serialization and Storage

For sequences of ABC that see a great deal of reuse, whether in serialization or storage, it seems worthwhile to name the sequence and access it by name. Doing so can save bandwidth and space, and potentially improve use of cache, memoization, compilation. ABC has support for naming external resources by use of capabilities. I aim to establish a convention, a common approach for using secure hashes to serve as identifiers for a body of ABC code, like so:

        {#SecureHashOfBodyOfAwelonBytecodeInBase64}

The meaning of this code is quite simple: inline expansion of the referenced body of code. Capability invocations always apply inline (access to the full tacit argument) so this falls within the normal semantics of capabilities. The referenced code may contain more such references. This behavior falls within the normal semantics of capability invocations. It is suggested that most ABC environments provide support for this feature by default. 

Presumably, one could use URLs or some model of names other than a secure hash. However, URLs would introduce complications regarding cyclic definitions, update and versioning, reuse of names across services, and so on. Secure hashes provide globally unique names, prevent cycles, separate the update concern, and enable flexible replication, storage, and content-based lookup processes. Hashes also enable validation. Use of a secure hash is simply better design.

This technique can be augmented further with cloud storage or p2p distribution. In these cases, we may wish to prevent peeping from the distribution service, which suggests use of encryption. To standardize this convention, the following format is suggested:

        {#secureHash:secureRandomEncryptionKey}

The secure hash is computed on the *encrypted form* of the code. Lookup uses only the secure hash, not the key. A fresh secure-random key is chosen for every encoded object. The key is always distributed in the above format, tightly coupled to the identifier. Some random junk may also be added to the code to further protect against attacks based on similar openings, e.g. via constructing a text literal then dropping it. Naturally, when code is distributed in this form, the ABC stream should also be encrypted.

I expect this technique to be pervasively used in Awelon project:

* SHA2-256 will be used for hashes
* AES-256 will be used for keys; secure-random key per value
* Encoded in Base64url (`A-Z` `a-z` `0-9` `-_`)
*   43 characters will encode 258 bits
*   pad two bits with `00`
*     doubles as version tag in case SHA2 or AES compromised
* Thus total encode is:
*   46 characters without encryption
*   90 characters with encryption

This technique should be used only where considerable space savings can be achieved. For optimizing smaller strings of code, simpler recognition and interning models could be used. One might use a convention based on 'words' (separated by spaces or lines) of ABC code to help recognizers.

When ABC code is used for storage, the potentially exists to gradually, continuously, refactor and refine codebases to improve compression, reuse, performance, and so on. Similarly, services may automatically factor ABC code based on global knowledge of which reuse is effective in practice.

*Thoughts:* In the unlikely case of a hash collision, we could disambiguate based on type safety. This only costs a little, and should result in a much more robust system even if SHA2 is compromised. Naturally, any such collisions should also be reported.

### Convention: Annotations!

Through capabilities, ABC supports a notion of "annotations". Annotations provide hints to a compiler, optimizer, theorem prover, or debugger. However, annotations must not impact the observable, formal semantics of the program. That is, removing the annotation should not change the meaning of the program.

Example applications for annotations:

* suggest block attribute: lazy or parallel
* suggest block attribute: compute on GPU or FPGA
* suggest sequencing of potentially lazy value
* suggest use of memoization or caching
* suggest specialization or JIT at points in code
* provide hints for proving termination
* improve blame, error, warning messages
* tracing annotations, track to original code
* compile-time traces, run-time debug logs
* debugger integration - breakpoints, visualization 
* report features when streaming, e.g. support for secure hashes
* estimate heap sizes, quotas, tolerances for process control

Annotations can be very useful despite their lack of formal meaning within the program. Developing a great set of annotations will take much convention and de-facto standardization.

If an ABC system doesn't understand an annotation, it should ignore it rather than raise an error. If part of an ABC processing pipeline, it should pass the annotation on unchanged since it might be meaningful in a later stage.

### Capabilities Under-the-Hood

Capability text can be generated lazily, when it is actually observed via serialization or reflective capability. In practice, many capabilities will never be observed, and thus the capability text never needs be generated. Capability invocations can essentially be replaced by object pointers. 

ABC can potentially distribute many capabilities by partial evaluation at compile-time. The indirection through a block can potentially be removed, the capability code inlined. 

### Pitfall: Conditional Capabilities (Don't Do It!)

When brainstorming, one of my ideas was a convention of the form `{?foo}`, whose output was a sum type depending on whether the environment recognizes `{foo}` as a valid capability. This seems neat because it results in code adaptable to the environment. 

However, the adaptation properties are simply *wrong*, lacking access to a big-picture view. The determination of whether a capability is available should be modeled at the point where it is acquired, where some logic can be introduced based on the set of available features. And pushing this upstream is better for testing, mockup environments, and configurations management. 

Also, the technique would be a bad choice for security reasons. We should never be guessing capability text. In retrospect, this idea was bad all around.

### Tail Call Elimination and Inlining

ABC can be implemented using a stack machine. Each frame on the stack would correspond to a block application, with `$` or `?`, which hides away part or the value and operates on the rest. 

There are many cases where we can easily inline code, e.g. if a block is applied by the pattern `v$c`. Inlining code can be a powerful technique for performance optimizations. Similarly, there are many cases were we might be able to reduce the stack size, e.g. if a block ends with `$]` or `$c]`, effectively achieving [tail call elimination](http://en.wikipedia.org/wiki/Tail_call).

An ABC implementation is not required to optimize tail calls, but it is encouraged. 

### Linear Implementations vs. Garbage Collection

In ABC, use of drop and copy operations (`%` and `^`) is explicit. We effectively have explicit memory management, but with pure, immutable values. This opens many choices for the implementation:

* linear, deep copy up front, no garbage
* alias on copy, track refcounts, copy on non-linear write
* immutable values in memory, implement ABC as pure functions

Some designs will be more or less efficient than others, or more or less amenable to various optimizations (laziness, interning, memoization, etc.). ABC doesn't specify any particular implementation, and it might be worthwhile for a compiler to experiment empirically and use different techniques for different volumes of code. 




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



