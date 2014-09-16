# Awelon Bytecode

Awelon Bytecode (ABC) is a bedrock component of the Awelon project. ABC consists of about forty operators, embedded text, blocks of ABC (to model first class functions), and a [capability secure](http://en.wikipedia.org/wiki/Capability-based_security) model to access external effects. The effects model is further leveraged to support dynamic linking, separate compilation, and performance annotations. 

**Why a new Bytecode?**

Awelon project has several concepts that are not well supported by existing languages or bytecodes. Among these: Awelon is aimed at open distributed systems, which requires special attention to security, serializability, aliasing, and linking. Awelon project includes Reactive Demand Programming (see [AboutRDP](AboutRDP.md)) which benefits from declarative optimizations and equational reasoning at the bytecode level. Awelon project aims to unify the user and programmer experiences, such that objects are readily shared and directly useful as software components, and even user input is modeled as a continuous stream of code that can be mined for tools and user macros.

**Distinguishing Properties**

ABC has many interesting or unusual properties that distinguish it from other bytecodes and programming languages.

* ABC is *streamable*. This means the bytecode can be incrementally executed as it arrives then quickly forgotten. We might stream bytecode to modify a web page, subscribe to a publishing service, or command a robot in real time. The bytecode can manipulate values and/or capability-based APIs. Streamability is essential to the Awelon project vision, to support new forms of tooling. For comparison, conventional bytecodes are designed for the [stored-program computer](http://en.wikipedia.org/wiki/Von_Neumann_architecture#History), and might 'jump back' to model a loop. ABC is streamed, not stored, and ABC does not jump. Loops are modeled instead using [fixpoint combinators](http://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator). 

* Unlike most bytecodes, ABC is *weakly legible*. Natural numbers have an obvious encoding, e.g. `#42` encodes the number forty-two. Text literals can be embedded directly. Basic operators use printable characters, visible in a text editor. Effects are accessed through visually obvious and often human-meaningful tokens between curly braces, e.g. `{foo}` (perhaps guarded by HMAC). In a suitable environment, even richly structured data - meshes, images, matrices, graphs, music notation, etc. - might be [embedded as renderable literal objects](doc/ExtensibleLiteralTypes.md) in ABC. The intention is to simplify learning ABC, debugging, disassembly, modification, and extraction of useful software components.

* ABC is almost purely *functional*. ABC's basic operators have purely functional semantics. ABC does not have a concept for references or aliasing. And ABC's value types are immutable. Further, side-effects are uniformly constrained to respect *spatial idempotence* and *causal commutativity*: invoking the same effect twice with the same argument must be the same as invoking it once, and ordering of effects is controlled only by the computation of arguments. Thus even effectful code allows a lot of nice equational reasoning (and optimizations) similar to pure code. 

* ABC supports a high degree of deterministic parallelism. Any block application (operators `$` or `?`) may be computed in serial or in parallel without changing the result of a valid program. The difficulty isn't one of finding opportunities for parallelism, but rather one of optimizing it. Developers may guide parallelism explicitly by use of annotations, and optimizers may inject parallelism decisions based on heuristics and profiling.

* ABC is multi-paradigm, but in a rather non-conventional sense. ABC can support *functional, imperative, or reactive* programming based on the compiler used and the set of effectful capabilities made available as arguments to the program. Valid optimizations for ABC are the same regardless of paradigm. A single ABC subprogram can often be reused for many different paradigms. 

* ABC is a [*tacit, concatenative* language](http://concatenative.org/wiki/view/Concatenative%20language), similar in nature to Forth, Joy, Factor, and Cat. Concatenating two valid ABC subprogram strings is the same as composing their functions, and it is trivial to extract subprograms into reusable software components. Though, unlike the aforementioned languages, ABC doesn't allow user-defined names (that's left to [higher level languages](AboutAO.md)) and is not stack-based (instead operating on products, sums, and numbers). 

* ABC is *strongly typesafe*, and amenable to static analysis. To the extent safety isn't validated statically, the runtime may enforce it dynamically. It is feasible to typecheck and infer safety for most ABC code, with minimal runtime checks at certain boundaries, e.g. when working with remote systems or side effects. 

* ABC supports [*substructural* typing](http://en.wikipedia.org/wiki/Substructural_type_system), in the form of adding affine (no copy) and relevant (no drop) attributes to blocks. This allows ABC to enforce structured programming behaviors without relying on a structured syntax.

* ABC is highly suitable for *open, distributed programming*. Arbitrary values can be serialized, communicated, and incrementally updated via streaming bytecode. Blocks are easily serialized and can model first class functions, mobile agents, or interactive applications. Capability secure effects and cryptographic value sealing can enable ad-hoc mashups of mutually distrustful subprograms and services. ABC's unusual separate compilation and linking model makes it easy to securely reuse code and data across independent services.

* ABC is highly suitable for staged programming. First class functions in ABC have a simple representation as blocks of ABC code, which makes it easy to recompile a function composed in an earlier programming stage. This is very useful for developing domain specific languages because they can achieve a high level of performance and compilation to native code. 


## The ABC Stream

ABC is represented in a stream of UTF-8 encoded characters. There are no sections, no headers or footers. There is just the stream, potentially unbounded in length. ABC is designed to be visible, printable. Text, blocks, invocations, and the encoding of numbers are at least weakly legible:

        [blocks[[are]nestable]]
        #42
        "text begins with a double-quote, and has a block-like format.
         text may extend multiple lines, each continued by LF SP (10 32)
         in which case the LF becomes part of the text. Otherwise, text 
         can be terminated by LF ~ (10 126), which adds no characters.
         ABC text has no need for escapes, other than SP after LF.
        ~
        {capabilityText}
        vrwlc

This visibility seems useful for didactic purposes and debugging. For similar reasons, ABC supports two whitespace characters (LF (10) and SP (32)) assigning to them the identity function (type `∀x.x→x`), to simplify formatting of ABC for human view.

## ABC Behavior Overview

The ABC stream contains operators, literals (blocks, text, numbers), and invocations.

### Operators

Each operator is represented as a single character, denoting a function. For example, operator `r` denotes the right association function, with type `((a * b) * c) → (a * (b * c))`, and `w` denotes the stack swap operator `(a * (b * c)) → (b * (a * c))`. ABC has a fixed finite alphabet of operators.

ABC operators are applied in sequence to an implicit value external to the ABC stream. This value is typically a composite, constructed of `(a * b)` pairs. This value may represent documents, a stack-based computing environment, or many things. 

ABC operators manipulate a small part of this structure using a stack-like metaphor whereby `(a * (b * (c * ...)))` represents a stack with top three elements `a`, `b`, and `c`. Deep manipulations are modeled by shuffling relevant elements to the top of the stack, manipulating them, then shuffling them back to their proper location. (Idiomatically, ABC paragraphs should start and end with elements in proper location.) *NOTE:* data shuffling can become expensive if ABC is interpreted, but a great deal can be eliminated if ABC is compiled.

Developers can legitimately comprehend juxtaposition as functional composition. The ABC sequence `rw` has type `((a * b) * c) → (b * (a * c))`. Conversely, an ABC program can easily be decomposed into valid subprograms and software components.

### Literals

Blocks and text are the literals of ABC. They require special attention by the ABC reader and result in values added to the stack. Numbers are a pseudo-literal. Literals can be understood as functions that introduce a value.

A block contains a finite sequence of ABC code, and may be understood as a first-class function that simply introduces its value. E.g. `[rw]` has type `x -> [((a * b) * c) → (b * (a * c))] * x`. Blocks are the foundation for loops, conditional behavior, and higher order programming. Blocks also support security, protecting interests of both provider (by encapsulating information or authority) and user (by constraining access on apply: `$ :: [x→x']*(x*e)→(x'*e)`).

Numbers use operator `# :: e → (N(0)*e)` to introduce a new zero, then each digit `0-9` has meaning of the form `3 :: N(x)*e → N(10x+3)*e`. Thus, numbers aren't literals, but natural numbers such as `#123` are close enough for legibility. Rational numbers are produced through operations on natural numbers, e.g. `#2#3/*` is two thirds.

Text is shorthand for producing a list of small numbers between 0 and 1114111 (0x10ffff), the Unicode codepoints. A text list has fixpoint type `µL.((c*L)+1)`, where `c` is a codepoint value. Binaries are encoded using a non-conventional base16 text. 

### Capability Invocations

Effects in ABC are achieved by invoking environment-provided operators: `{foo}` invokes the environment with token "foo" and the tacit argument. 

Tokens are *unforgeable* from within ABC. That is, given the string "foo", there is no primitive ABC operator to construct invocation `{foo}`. In an open or distributed system, a token should also be protected from external forgery. This is typically achieved by cryptographic mechanisms, e.g. encryption, secure hashing, signatures, or generation of securely random numbers. 

By capturing an invocation within a block, e.g. `[{foo}]`, we effectively have first-class but unforgeable access to whatever effect the invocation achieves. This is a capability, and is suitable for secure programming under the [capability security model](http://en.wikipedia.org/wiki/Capability-based_security). This is useful in open and distributed systems, which are primary target areas for ABC. Consequently, an invocation token is sometimes described as capability text. 

Effectful tokens are typically specific to a virtual machine or runtime environment. However, there are some standard conventions for the special exceptions where we want a token to have a common meaning across independent runtimes in a distributed system. These are usually identified based on the first character:

* `{&ann}` - annotation, identity behavior, for performance and debugging.
* `{:seal}` and `{.seal}` - sealers and unsealers for rights amplification.
* `{#hashOfCiphertext:hashOfBytecode}` - link and load, separate compilation

These are discussed with more detail in later sections.

*Note:* tokens may not contain characters `{`, `}`, or LF (10). 

## ABC Behavior Details

### Basic Data Shuffling

ABC provides a minimal set of primitive operators for block-free structure manipulation. The primary structure in ABC is the product (pair), type `(a * b)`.

        l :: (a * (b * c)) → ((a * b) * c)
        r :: ((a * b) * c) → (a * (b * c))
        w :: (a * (b * c)) → (b * (a * c)) 
        z :: (a * (b * (c * d))) → (a * (c * (b * d)))
        v :: a → (a * 1)
        c :: (a * 1) → a

Type `1` is identity for product, called 'unit', and provides static structure.

There are other minimal sets with fewer operators, but this set has some nice symmetry properties. The operators `lzrw` are sufficient for all data shuffling where the rightmost element is sticky, and `v` can displace the rightmost element.

Example encodings:
        
        zw    :: (a * (b * (c * d))) → (c * (a * (b * d))) -- rot3
        vrwlc :: (a * b) → (b * a) -- full swap

In addition to shifting objects around, we can drop or copy values:

        % :: (Droppable x) ⇒ (x * e) → e
        ^ :: (Copyable x) ⇒ (x * e) → x * (x * e)

In ABC, blocks can be tagged with substructural attributes that make them uncopyable or undroppable. But otherwise, all values are copyable and droppable by default. A product or sum may be copied if both element types may be copied.

### Blocks

A block in ABC contains a finite sequence of ABC. Blocks may be constructed as literals, by composition `o`, or by quotation `'`. 

        [vrwlc] :: e → ([(x * y) → (y * x)] * e)
        o :: [x→y] * ([y→z] * e) → ([x→z] * e)
            [def][abc]o = [abcdef]
        ' :: x * e → [s→(x*s)] * e
            #42' = [#42]
            [vrwlc]' = [[vrwlc]]

After construction, a block is applied with the `$` operator:

        $ :: [x→x'] * (x * e) → (x' * e)

Loops are modeled via fixpoint combinators, in particular the strict fixpoint combinator called the Z combinator. A simple Z combinator is `[^'o]o^'o`. A variation suitable for AO's multi-stack environment is `r[^'ol]o^'ol`. But note that loops should always terminate in Awelon project; we'll use higher layers (streaming, RDP) for long running services.

Higher order programming can be modeled as a block that expects a block as an argument. Currying (partial application) can be modeled by combining quotation with composition.

*Aside:* Arguments for `o` were recently swapped to match the most frequent use case (otherwise almost every instance of `o` is preceded by `w`), so there may be some errors in the documentation.

### Numbers

ABC's primitive number type is arbitrary precision rationals. In some cases - when the required range and precision or error properties are well understood - a compiler may substitute use of integers, fixpoint, or floating point numbers. 

Natural numbers can be expressed using pseudo-literal constructors in ABC:

        # :: e → N(0) * e
        0 :: N(x) * e → N(10x+0) * e
        1 :: N(x) * e → N(10x+1) * e
        2 :: N(x) * e → N(10x+2) * e
        ...
        9 :: N(x) * e → N(10x+9) * e

After construction, numbers can be manipulated by a few elementary operations: add, multiply, their inverses, and divmod (to simplify inference of precision and modulus):

        + :: (N(a) * (N(b) * e)) → (N(a+b) * e)
        * :: (N(a) * (N(b) * e)) → (N(a*b) * e)
        - :: (N(a) * e) → (N(0-a) * e)
        / :: (N(non-zero a) * e) → (N(1/a) * e)
        Q :: (N(non-zero b) * (N(a) * e)) → (N(r) * (N(q) * e))
            such that q integral, r in (b,0] or [0,b), and qb+r = a

Rational numbers must be computed through such manipulation. For example:

        #2#3/*-  (-2/3)
        #123/00/ (1.23)

ABC is not rich in math, nor especially efficient at it. High performance graphical or scientific computing should often be handled indirectly, modeled symbolically and compiled for OpenCL or GPU. I imagine that, eventually, widely used math operations will be well supported by ABCD.

*NOTE:* Awelon project systems should eventually track dimensionality properties for most numbers (e.g. 5 kilograms vs. 5 meters). Doing so offers pretty good semantic context and safety, and nicely fit structural types. There are no primitives for this; just convention. See AboutAO for more.

### Text

The ABC stream can contain arbitrary blocks of unicode text:
        
        "text has a block format 
         it starts with double quote
         it may continue on multiple lines
         each ending with linefeed (10)
         which is escaped by following space
         or terminates text with tilde (126)
        ~

If anything other than space or `~` follows LF, the ABC stream is in error. There are no escape characters in ABC, except for SP to escape a preceding LF. By convention, text starts at a new line to keep it more readable. Text is less aesthetically pleasing, but still legible, when used for a single line or word:

        "Text
        ~

Text is not a distinct type for ABC. Rather, text is understood as a compact representation for introducing a static list of small integers (range 0..1114111, from UTF-8). The standard model for a list is: `µL.((element*L)+1)`.

ABC does not have a 'binary' literal type, but developers may use a specialized base16 encoding that is recognized by the compression pass. See the section on Binaries, far below.

*NOTE:* ABC's representation of text is simplistic. Real text manipulation demands precise knowledge of the characters (ligatures, combining marks, etc.), and benefits from a more sophisticated representation than a flat list of numbers. However, ABC's representation of text is sufficient for identifiers, embedded DSLs, and so on.


### Identity

ABC treats two whitespace characters - SP (32) and LF (10) - as identity operators with type `x → x`. Effectively, whitespace in the ABC stream may be ignored. Also, the empty program is equivalent to identity, since it performs no operations on the tacit input.

*Note:* Other whitespaces - e.g. tabs and carriage returns - are not valid ABC operators. If encountered in an ABC stream (outside of text), an error must be raised, as for any other invalid operator.

### Substructure

[Substructural types](http://en.wikipedia.org/wiki/Substructural_type_system) are interesting because they can express structured control-flow or dataflow orthogonally to syntax. They can enforce that protocols and handshakes complete, that resources are released, that promises are kept, that callbacks or continuations execute once and only once. Substructure is also useful for modeling uniqueness or ensuring exclusive write access to a resource.

In ABC, only blocks have substructural type. This is represented by marking an existing block with substructural type:

        k :: ([x→y] * e) → ([x→y]' * e) -- relevant, no drop
        f :: ([x→y] * e) → ([x→y]' * e) -- affine, no copy

These operations are naturally idempotent and commutative.

An affine block may not be copied. A relevant block may not be dropped. A linear block may not be copied or dropped. However, a relevant or linear block may still be applied with `$`.

When blocks are quoted or composed, or a structure containing blocks is quoted, the composite inherits substructural attributes of all the components: 

        [code]f [more]k o = [morecode]kf
        [code]f [more]  o = [morecode]f
        [code]k [more]fl' = [[code]k[more]fl]kf
        [code]f' = [[code]f]f

When a relevant block is copied, both copies are relevant. (*Note:* Technically, only one of the two copies must be relevant. However, it is difficult to explain this in a type system, and difficult to track in a streaming scenario. For simplicity, both copies are relevant.)

### Conditional Behavior

A sum type, `(a + b)`, represents that we're either right with `b` or left with `a`. Convention is to pun 'right' with 'true', i.e. `(false+true)` ordering. A sum type is the typical result of making an observation, such as comparing two numbers:

        > :: N(x) * (N(y) * e) → ((N(y)*N(x))+(N(x)*N(y)) * e -- y > x
            #4 #2 > -- observes 4 > 2. Returns (N(2)*N(4)) on right.

Technically, `>` is the only observer operator built into ABC. However, it is assumed that invocation of capabilities will often result in sum type observations, e.g. indicating failure vs. success. Sums are also used to model algebraic data structures, e.g. a list can be modeled with `µL.((element*L)+1)`.

Sum types have their own set of data shuffling operators:

        L :: (a + (b + c)) * e → ((a + b) + c) * e
        R :: ((a + b) + c) * e → (a + (b + c)) * e
        W :: (a + (b + c)) * e → (b + (a + c)) * e
        Z :: (a + (b + (c + d))) * e → (a + (c + (b + d))) * e
        V :: a * e → (a + 0) * e
        C :: (a + 0) * e → a * e

Type `0` is identity for sum, called 'void', and corresponds to a vacuous condition. Static analysis may infer types for void to reject inconsistencies even in dead code. After a condition is observed, we can conditionally apply a block:

        ? :: (Droppable b) ⇒ b@[x→x'] * ((x + y) * e) → (x' + y) * e

A block with the 'relevant' substructural property cannot be applied in this manner. Any such block must be applied under all conditions. In addition, we can distribute, factor, and merge sums:

        D :: a * ((b+c) * e) → ((a*b) + (a*c)) * e -- distrib
        F :: ((a*b) + (c*d)) * e → (a+c) * ((b+d) * e) -- partial factor
        M :: (a+a') * e → a * e -- merge

Full factor is modeled by combining partial factor and merge:

        FM :: ((a*b)+(a'*c))*e → a*((b+c)*e) -- full factor; inverse of D

On merge, the types `a` and `a'` must be compatible for future operations. What 'compatibility' requires may be judged in context, knowledge of future operations. 

Sums may also be copied or dropped (with `^` and `%`) assuming both element types may be copied and dropped.

*ASIDE:* A possible type checking technique is to have a 'merged' type that simply validates future operations on both paths. In practice, the partial factor operation can be more difficult to precisely type, because we easily lose precise information about how `(a+c)` and `(b+d)` are on the same side. 

### Partial Functions and Contracts

ABC provides a simple operator for partiality, assertions, and contracts:

        K :: (a + b) * e → b * e

This operator represents a form of divergence: if we're in the left, that's equivalent to a type error and we'll stop the program as quickly and cleanly as possible. Otherwise we're okay. However, ABC is not restricted to runtime detection of this error. Expressing partial functions with `K` enables ABC to infer dependent types and contracts. If `K` cannot be statically proven safe, the programming environment may issue a warning or raise an error.

## ABC Assumptions and Idioms

This section discusses a few high level properties of ABC's design and context that cannot readily be inferred from the earlier operators or tacit concatenative streamable structure.

### Causal Commutativity and Spatial Idempotence

ABC requires causal commutativity and spatial idempotence for effects models.

Causal commutativity means that there is no ordering relationship unless there is a visible dependency where an output of one behavior is observed or manipulated by the next. This property is valuable for optimizations (cf. [Causal Commutative Arrows and their Optimization](http://haskell.cs.yale.edu/?post_type=publication&p=72) by Hai Liu, Eric Chang, Paul Hudak). 

        conventional commutativity: (ABC does not generally have)
            foo bar = bar foo
        causal commutativity: (ABC requires and assumes)
            [foo] first [bar] second = [bar] second [foo] first
              where first  :: [a→a'] * (a*b) → (a'*b)
                    second :: [b→b'] * (a*(b*c)) → (a*(b'*c))

Spatial idempotence means that, if the same action is performed twice with the same inputs, there is no additional observable impact. This property is also extremely valuable for optimizations, e.g. in content distribution networks. 

        conventional idempotence: (ABC does not generally have)
            foo foo = foo
        spatial idempotence: (ABC requires and assumes)
            [foo] first dup = dup [foo] first [foo] second
              where first  :: [a→a'] * (a*b) → (a'*b)
                    second :: [b→b'] * (a*(b*c)) → (a*(b'*c))
                    dup    :: (x * e) → (x * (x * e))

ABC is designed primarily for reactive demand programming (RDP), which naturally has both spatial idempotence and causal commutativity. By requiring these properties, ABC code can be uniformly optimized without tracking usage context. 

Fortunately, it is not difficult to enforce spatial idempotence and causal commutativity even for imperative programming styles. Primarily, one favors linear objects such that duplicate effects cannot be expressed and effects for each object are serialized. Where necessary, capability secure [oracle machine](http://en.wikipedia.org/wiki/Oracle_machine) idioms allow controlled expression of race conditions.

Spatial idempotence and causal commutativity offer valuable properties for equational reasoning and optimizations. ABC programs can be manipulated in many ways similar to pure functions.

### Fast and Loose Reasoning for Termination

ABC favors a philosophy of 'fast and loose reasoning' about termination properties. (cf. [Fast and Loose Reasoning is Morally Correct](http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html), Danielsson, Hughes, et al. POPL 2006.) The idea is that - with respect to optimizations, equational laws, rewriting, loop fusion, parallelization, laziness - we should assume every subprogram terminates (or is intended to). 

To help enforce and remind developers of this philosophy, ABC compilers should perform termination analysis, and long-running programs should be modeled as RDP behaviors or incremental processes (e.g. `μProc.[a→(Proc*b)]`).

ABC is Turing complete, so we can't expect a decision to be reached for every program. However, termination analysis is really about avoiding errors in reasoning. And *most errors aren't subtle*. Accidental non-terminations often can be detected, and many developers will write reusable software such that analysis passes without a 'could not prove' warning.

Termination is a weak property. In practice, we often wish to reason about performance characteristics: latency, memory requirements, resource usage. Ackermann function terminates, but not within reasonable bounds. However, termination is a great start.

### Flexible Safety Analysis

ABC has an implicit type system with six structural types (pair, sum, unit, void, number, block), substructural types (relevant, affine, expires), and modal types for spatial-temporal attributes (latency, location, sealed values). Termination analysis is recommended to validate fast-and-loose reasoning. ABC further supports inference of dependent types or contracts by use of operator `K`.

However, ABC does not specify any inference algorithms. I hope instead to enable independent evolution of analysis, a growing array of recognizers and strategies to prove safe structures, algorithms, and architectures. 

Instead, ABC has a philosophy:

* prove it right, pass it silently
* prove it wrong, raise an error
* indecision, pass with a warning and check at runtime
* incrementally improve strategies to efficiently decide more programs

Most static languages reject programs unless they're provably correct according to the type system. ABC's more relaxed philosophy accepts (with warning) code that is not provably wrong. This enables analysis to vary between lightweight and heavyweight. 

It also may give ABC a more dynamic feel, especially combined with dependently typed structures. ABC's philosophy is close in nature to [gradual typing](http://en.wikipedia.org/wiki/Gradual_typing), albeit with more inference and less annotation.

### Annotations as Capabilities

Annotations are expressed as capability text using prefix `&` as in `{&asynch}`. Annotations must not impact the observable, formal semantics of a valid program - i.e. a valid program should behave equivalently if any given annotation is removed. However, annotations are useful for debugging, performance suggestions, and to help identify invalid programs. 

* suggest use of parallel evaluation; `{&asynch}`
* suggest dynamic compilation of a block; `{&compile}`
* suggest use of memoization or caching
* hints for typing safety or proving termination
* assert structural equivalence of two values; `{&≡}`
* improve blame, error, and warning messages
* breakpoints, location info, console traces

The intention with annotations is that they can be rather ad-hoc, and that ABC runtimes may ignore those it does not recognize. When processing ABC through optimization pipelines, unrecognized annotations should generally be passed forward.

### Spatial-Temporal Types and Capabilities

ABC is designed for RDP, and RDP's design leverages a model of spatial-temporal types in a context of programming overlay networks, heterogeneous computing, and distributed systems. For security reasons, these spatial-temporal types cannot be directly observed or manipulated by ABC primitives. However, they may be influenced or observed by capability invocations.

A 'spatial' type is essentially a description of *where* a value is. This includes physical locations with varying precision - server, client, GPU, FPGA, specific threads. Additionally, virtual or logical locations may be modeled to simplify reasoning about interactions between subprograms, or to model staging or pipelines.

A 'temporal' type is a description of *when* a value can be observed, and might be described as a rational number of seconds from program start. Temporal types are useful to control reactive feedback loops and to understand and manage latencies in distributed systems. Upper bounds - expirations - are also useful. They help model timeout protocols, control distribution, and interact in interesting ways with substructural types.

The space-time model is a matter of convention, with a de-facto standard that is not part of ABC's definition. It is enforced through the capabilities protocols. My current vision has the following characteristics:

1. Adding two numbers, comparing two values, quoting a structure, etc. requires all inputs coexist in space-time. Some delay for adding or comparing numbers may be implicit, but movement is generally explicit. 
2. Temporal manipulations use a 'temporal cursor' concept: rather than delaying values directly, you advance values to a cursor, which can be manipulated independently. (The idea is to maximize idempotence and commutativity of delay operations.)
3. New substructural types may be introduced, notably *expiration* for blocks. A block can be marked for expiration, such that it is an error to apply it after a known point in time. For relevant blocks, it may further be an error to delay it beyond that point. Similarly, blocks may be specific to location.
4. Spatial manipulations are very specific, i.e. each capability representing a *directed edge* between spaces in a connectivity graph. Developers can constrain the connectivity graph in useful ways, e.g. to enforce staging of computation. 
5. For security reasons, reflection on spatial-temporal types is separate from distribution and manipulation. In general, we don't want most code to be sensitive to where it executes.

The intention is that these capabilities are distributed *statically*, i.e. using partial evaluation or a staged model, such that the spatial-temporal information is available at compile-time. Also, while spatial-temporal types and capabilities are intended primarily for RDP, I expect they would be useful for imperative programming.

*NOTE:* Distributed systems must admit the possibility of non-deterministic disruption. In many cases, it can be useful to model distribution capabilities as having the possibility of failure. 

*NOTE:* Physical resources are almost always location specific, and must be manipulated from the right location. The idea is to put code near the resource, rather than to remotely manipulate it.

### Uniqueness Types and Capabilities

Modeling creation of 'new' or 'unique' values requires special consideration in ABC. Due to spatial idempotence, two equivalent requests for a 'new' value must return the same result. Consequently, it is necessary to ensure that each request for a unique value is a unique request.

Linear and affine types are useful in this endeavor. ABC systems can model a 'uniqueness source' - a unique capability that generates unique values on demand. While a uniqueness source cannot be copied (because it would no longer be unique!), it may be *forked* such that there are now two unique uniqueness sources. An operation to construct a unique value will consume the uniqueness source, so it becomes necessary to iterate between forking and construction. 

New unique constructs will typically be one of:

* a sealer/unsealer pair (see Value Sealing, below)
* exclusive capabilities to access a state resource
* a cryptographically secure pseduo-random number generator 

Exclusive state capabilities correspond to conventional allocations, e.g. to `newIORef` in Haskell. For some state models (specifically, those that also respect *causal commutativity*, though this excludes most imperative state models) it may be permissible to share access to the state after obtaining the initially exclusive capabilities. 

In many contexts - live programming, orthogonal persistence, open systems - it is useful to ensure *stability* of unique values. 

Stability is readily achieved using a filepath/URL metaphor: when 'forking' a uniqueness source, we provide an identifier for the child that is unique within the parent. This stabilizes uniqueness with respect to changes in the order children are constructed. If used with a little discipline, significant restructuring of the source code becomes feasible without damaging persistence or relationships assuming stable identity.

### Value Sealing Types and Capabilities

Value sealing is a simple technique with very wide applications. The setup is simple. We have two capabilities - a 'sealer' and the corresponding 'unsealer'. At runtime, these may be allocated as a pair, using a secure uniqueness source (see above). 

        {:u} :: a → Sealed u a        `:` for seal
        {.u} :: Sealed u a → a        `.` for unseal

Some sealers will be weak, discretionary tags like `{:foo}`, which serve a useful role as something like a structural type tag to resist accidental coupling to internal data structures or provide hints to a rendering engine. Other sealers may be very strong, using cryptographic keys. We'll reserve symbol `$` for cryptographic sealers:

        {:format$leftKey}             cryptographic sealer
        {.format$rightKey}            cryptographic unsealer
        {$format}                     indicate sealed value

Here, format might be something like 'ecc.secp256k1', indicating how the argument is encrypted or decrypted. The format may be blank to use the defaut. The proposed default is ecc.secp256k1, mostly because it is used by Bitcoin. Elliptic curve cryptography offers asymmetric encryption with relatively small keys compared to RSA.

Serialization formats for discretionary vs. cryptographic sealed values:

        #42 [{:foo}]$                 discretionary sealed value, clearly 42
        ["cipherText\n~c]f{$fmt}      cryptographically sealed affine value

For discretionary values, we'll serialize values directly into ABC then seal it again at the remote host. Discretionary sealed values are generally accessible to reflective and introspective capabilities even without properly unsealing them.

For cryptographically sealed values, we'll instead serialize into text, then compress and encrypt it similar to an ABC resource. We wrap this in a block to preserve substructural types (affine, relevant, linear, expiration, etc.), and we wrap the value to indicate a cryptographically sealed value and protect the substructural types. The cipherText may contain a proper checksum. In general, cipher texts and keys are encoded in ABC's base16 format to leverage the special compression pass (see encoding of binaries in ABC, below). 

If a value is never serialized, or is serialized only between trusted machines (or machines we already know to possess the unsealer) then we might forego the encryption step. 

Value sealing is an important companion to object capability security. It provides a basis for [rights amplification](http://erights.org/elib/capability/ode/ode-capabilities.html#rights-amp), whereby you can gain extra authority by possessing both a capability and a sealed value, or by unsealing a value to gain a capability. It can also enforce various modularity patterns even in distributed systems. Value sealing should be considered orthogonal to transport-layer encryption. 

### ABC Resources for Separate Compilation and Dynamic Linking

The most direct way to reuse code in ABC is simply to repeat it. But reuse by repetition can be very inefficient for bandwidth and storage, and can hinder caching and compilation. 

A well understood alternative to repeating a large code structure is to simply name it once then repeat the name. When the name is smaller than the resource, this results in space and bandwidth savings. Further, even if the name is a bit larger than the resource, reuse of the name might amortize auxiliary processing costs. Names are thus excellent opportunities for caching, compilation, and so on.

Unfortunately, conventional approaches to naming introduce their own problems: name collisions, cycles, update and cache invalidation, location dependence, and version consistency issues. These misfeatures can be troublesome for security, safety, streaming, and distributed programming. Fortunately, we can address these problems by adopting a more rigorous, automatic naming system. Instead of human-provided names, we compute a cryptographically unique name by applying a secure hash to the bytecode. 

We then use ABC's effects model to invoke the named resource as needed:

        {#secureHashOfBytecode}         (preliminary! not used!)

This invocation tells the runtime to obtain the named resource and logically inline the associated Awelon bytecode. Obtaining a resource might involve downloading it. Logically inlining a resource might involve compilation to machine code and a dynamic linking. Either of these steps might fail, in which case the program fails (early and gracefully, if efficiently feasible).

This preliminary naming model has a weakness: the bytecode is exposed to the storage service. Thus, developers must be especially careful about the distribution of 'sensitive' bytecode (for security or privacy or intellectual property reasons). This is inconvient. Ideally, we should be able to use content distribution networks, peer-to-peer distribution, cloud storage, and similar without confusing security concerns!

Fortunately, this weakness is easily addressed. We simply encrypt the bytecode and add a decryption key to the name. To keep the naming system simple and deterministic, we use a secure hash of the bytecode for the decryption key, and a secure hash of the ciphertext for the lookup key. We can authenticate by the same hashes. In addition, since we cannot compress after encryption, we might want to compress to save bandwidth and storage. Some relevant pseudocode:

        encryptionKey = secureHashBC(bytecode)
        cipherText = encrypt(compress(bytecode),encryptionKey)
        lookupKey = secureHashCT(cipherText)
        store(lookupKey,cipherText)

        invoking {#lookupKeyEncryptionKey}
            i.e. {#hashOfCiphertextHashOfBytecode}

I would expect most resources range about three orders of magnitude, i.e. from hundreds of bytes to hundreds of kilobytes. They might go bigger for large data objects, e.g. 3D models, texture and material models, sound models. The repetitive data plumbing patterns of ABC should compress very effectively. 

Algorithmic details are not fully settled. Thoughts:

* want a simple, unambiguous, deterministic specificiation
* secure hash BC: last 256 bits of SHA3-384 
* secure hash CT: first 128 bits of SHA3-384
* specialized base16 encoding of resource id (see below)
* encryption: AES in CTR mode, simply using a zero nonce/IV
* authenticate and filter ciphertexts using both secure hashes
* compression should support embedding large binary data (see below)

For ABC resources, we require *deterministic* compression - i.e. the same input always results in the same compressed output, without heuristic 'compression levels' or similar. An appropriate [compression algorithm](doc/Compression.md) is still under consideration.

*ASIDE:* A remaining vulnerability is confirmation attacks [1](https://tahoe-lafs.org/hacktahoelafs/drew_perttula.html)[2](http://en.wikipedia.org/wiki/Convergent_encryption). An attacker can gain low-entropy information - e.g. a bank account number - by exhaustively compressing and hashing candidates and confirming whether the name exists in the system. To resist this, a compiler should add entropy to potentially sensitive resources via annotation or embedded text. Distinguishing sensitive resources is left to higher level languages and conventions, e.g. in AO we define word `secret!foo` for every sensitive word `foo`.

### ABC Paragraphs

ABC encourages an informal notion of "paragraphs" at least in a streaming context. A paragraph separates a batch of code, serving as a soft indicator of "this is a good point for incremental processing". A well-behaved ABC stream should provide relatively small paragraphs (up to a few kilobytes), and a well-behaved ABC stream processor should respect paragraphs up to some reasonable maximum size (e.g. 64kB) and heuristically prefer to process a whole number of paragraphs at a time. The batch would be typechecked, JIT compiled, then (ideally) applied atomically. 

A paragraph is expressed by simply including a full, blank line within ABC code. I.e. LF LF in the toplevel stream outside of any block. This corresponds nicely to a paragraph in a text file. Formally, the space between paragraphs just means identity. Paragraphs are discretionary. The reason to respect them is that they're advantageous to everyone involved, i.e. for performance and reasoning.

### Encoding Binaries in ABC

Programmers often work with binary encoded data, e.g. compressed visual or audio data, secure hashes, ciphertext. I would like to encode MP3 files, texture data, or short video clips as ABC resources. This would allow me to leverage ABC's secure content distribution, caching, partial evaluation, and nearly transparent link model. However, unless embedded binaries can be stored and transmitted efficiently, this simply won't happen.

ABC does not have an embedded literal type for binaries. However, ABC resources and network streams will be compressed, and perhaps we leverage that. The idea for encoding binaries in ABC is simple:

1. naively encode binary data in a base16 alphabet
2. specialize a compression pass to recognize base16

The compression algorithms under consideration[*](doc/Compression.md) would encode large binaries with about 0.8% overhead prior to the main compression, and with another 1:64 overhead if the binary itself cannot be compressed. This would be acceptable for almost any application. And even smaller binaries, like resource IDs (48 bytes), are encoded with a reasonable 4% overhead.

I plan to use a non-conventional base16 alphabet: `bdfghjkmnpqstxyz`. This is the lower case English alphabet minus vowels `aeiou` and most ABC data plumbing operators `vrwlc`. This alphabet ensures this specialized compression primarily impacts intentionally binary encoded data. It also avoids risk of spelling offensive words by accident. 

## Awelon Bytecode Deflated (ABCD)

I plan to develop a larger bytecode above ABC: ABC Deflated, or ABCD.

ABCD extends ABC with a dictionary - one standard dictionary for everyone - that maps higher UTF-8 characters (U+00C0 and above, reserving lower codes) to common, provably correct, widely used ABC subprograms. An ABC runtime will be expected to have a database of these definitions, and perhaps even have specialized optimizations for them. 

ABC streams may then be compressed against this dictionary, or alternatively generated using these operators directly. Further, ABCD interpreters can include specialized implementations for many of these functions. For many functions, a specialized implementation could be much more efficient than interpreting the implementation or even using a generic compiler. Further still, the selected functions could have well understood equational laws to simplify rewrite optimizations. For example, if a function reverses a list, then we know applying it twice results in the input. And if a function maps over a list, then mapping two functions is equivalent to composing the function and mapping once.

Development of ABCD shall be incremental and empirical, driven by actual data from real applications, with attention to popular data structures and usage patterns. Unicode is big (over a million elements) and realistically the standard dictionary will be much smaller so we can still have small runtime implementations. I would be surprised if we ever use 0.5% of the available space (~5000 functions). We should gain considerable benefits from much less than that.

ABCD complements ABC's resource model for separate compilation and dynamic linking. ABCD is suitable for relatively short, widely used functions. ABC resources are suitable for project specific software components and large data objects (to leverage content distribution networks and caching). These two techniques fill very different niches, and between them ABC may be minimal with little concern for performance or parsimony.

