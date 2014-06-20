# Awelon Bytecode

Awelon Bytecode (ABC) is a primary component of the Awelon project. ABC is a streamable, securable, type-safe, tacit concatenative, non-strict, causally commutative, spatially idempotent, weakly legible, functional bytecode. Breaking this down:

* **streamable** supports incremental processing; no sections or addressing
* **securable** effects via invocation of unforgeable capability text
* **typesafe** types can enforce many safety properties, and can be inferred
* **tacit** no local variable or parameter names; ops apply to environment
* **concatenative** juxtaposition is composition; also trivial to decompose
* **non-strict** no commitment to common evaluation orders; temporal control
* **causally commutative** effect ordering is expressed by argument threading
* **spatially idempotent** duplicate expression doesn't duplicate effect
* **weakly legible** visible, formattable code; readable text and numbers
* **functional** higher order expressions, immutable values, pure by default
* **bytecode** UTF-8 for text, but ABC codes within Latin-1 character set

ABC is suitable for functional, procedural, and reactive programming. ABC is primarily designed for reactive demand programming (RDP). ABC can be interpreted, but is intended for compilation or JIT. ABC supports linking and invoking external code via `{#secureHash}`, as an effective basis for structure sharing and cachable compilation.

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

This visibility seems useful for didactic purposes and debugging. For similar reasons, ABC supports two whitespace characters (LF (10) and SP (32)) assigning them the meaning 'identity', to simplify formatting of ABC.

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

Text is shorthand for producing a list of small numbers between 0 and 1114111 (0x10ffff), the Unicode codepoints. A list has fixpoint type `µL.((element*L)+1)`. 

*NOTE:* ABC has no support for binary literals, but text can encode LZHAM compressed base-64 with reasonable efficiency.

### Capability Invocations

Effects in ABC are achieved by invoking environment-provided operators: `{foo}` invokes the environment with token "foo" and the tacit argument. 

This token is unforgeable from within ABC. There are no operators to invoke the environment with computed text. In potentially open or distributed system, the token should also be unforgeable from outside of ABC. This feature is achieved by cryptographic means - e.g. encrypted text, signed text, or secure random GUID. 

Access to effects can then be [securely distributed](http://en.wikipedia.org/wiki/Capability-based_security) by wrapping invocations within a block like `[{foo}]`. In this role, blocks are often called capabilities.

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
        o :: [y→z] * ([x→y] * e) → ([x→z] * e)
            [abc][def]o = [abcdef]
        ' :: x * e → [s→(x*s)] * e
            #42' = [#42]
            [vrwlc]' = [[vrwlc]]

After construction, a block is applied with the `$` operator:

        $ :: [x→x'] * (x * e) → (x' * e)

Loops can be modeled with fixpoint combinators. A simple fixpoint combinator is `[^'wo]wo^'wo`, and a variation suitable for AO's standard environment is `r[^'wol]wo^'wol`. These apply to a block and cause it to construct itself (in fixpoint form) as an argument. *Note:* long running services or applications are not modeled by loops. Instead, incremental automata or reactive behaviors are defined assuming an *implicit* top-level loop, which is ultimately provided by the compiler.

Higher order programming can be modeled as a block that expects a block as an argument. Currying (partial application) can be modeled by combining quotation with composition.

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

*NOTE:* Awelon project systems shall generally track dimensionality properties for numbers. Doing so offers pretty good semantic content and safety, and nicely fit structural types. There are no primitives for this; just convention. See AboutAO for more.

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

When blocks are quoted or composed, or a structure containing blocks is quoted, the result inherits substructural attributes of all the components: 

        [code]f [more]k o = [codemore]kf
        [code]f [more]  o = [codemore]f
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

Annotations are expressed as capability text using prefix `&` as in `{&asynch}`. Annotations must not impact the observable, formal semantics of a program. Annotations may suggest or hint at performance attributes, but not enforce any properties. Despite these limitations, annotations are potentially useful for many ad-hoc purposes:

* suggest a block compute lazily
* suggest use of lazy, strict, or parallel evaluation
* suggest blocks be computed on GPU or FPGA
* suggest use of memoization or caching
* suggest specialization or JIT of a constructed block
* provide hints for proving safety or termination
* assert properties inaccessible to `K`, e.g. purity of a block or equivalence of two blocks.
* improve blame, error, or warning messages; add a warning
* track locations in original source code
* debugger support - breakpoints, location info, console traces

When an ABC subsystem doesn't understand an annotation, it should ignore it (treat it as an identity operator) rather than raise an error. If part of an ABC processing pipeline, it should pass the annotation on unchanged since it might be meaningful in a later stage. Annotations may be removed when aggressively optimizing.

### Spatial-Temporal Types and Capabilities

ABC is designed for RDP, and RDP's design leverages a model of spatial-temporal types to support modeling of overlay networks, heterogeneous computation, and distributed systems. 

A 'spatial' type is essentially a description of *where* a value is. This includes physical locations with varying precision - server, client, GPU, FPGA, specific threads. Additionally, virtual or logical locations may be modeled to simplify reasoning about interactions between subprograms, or to model staging or pipelines.

A 'temporal' type is a description of *when* a value can be observed, and might be described as a rational number of seconds from program start. Temporal types are useful to control reactive feedback loops and to understand and manage latencies in distributed systems. Upper bounds - expirations - are also useful. They help model timeout protocols, control distribution, and interact in interesting ways with substructural types.

Manipulations of spatial and temporal types are effectful and are performed through capabilities. Consequently, the exact model can be a matter of convention apart from ABC's definition. My current vision has the following characteristics:

1. Adding two numbers, comparing two values, etc. typically requires they coexist in space-time. Some delay for adding or comparing numbers may be implicit, but movement is generally explicit. 
2. Distribution of values (operator `D`) across a sum type are often constrained based on where the values are located or where the sum type may be observed. Conversely, location may contribute to compatibility of values on merge (operator `M`).
3. Temporal manipulations use a monotonic 'temporal cursor' metaphor. This cursor is a non-linear capability that may be advanced incrementally then applied to various values, delaying them or dooming them to expire. There may also be a method to synchronize cursors.
4. Spatial manipulations are specific and absolute, i.e. representing specific edges in a directed connectivity graph, not just 'goto location'. They may also be restricted on temporal coordinate. Constraints on connectivity are useful for staging, security, and modeling complex physical systems.
5. Capabilities to manipulate spatial-temporal attributes are separate from capabilities to observe/introspect the same. This restriction on information is useful for security and portability reasons.

The intention is that these capabilities are distributed *statically*, i.e. using partial evaluation or a staged model, such that the spatial-temporal information is available at compile-time. Also, while spatial-temporal types and capabilities are intended primarily for RDP, I expect they would be useful for imperative programming.

*NOTE:* Spatial manipulations in distributed systems must admit the possibility of non-deterministic disruption. In many cases, it is useful to model a 'connection' or channel in order to better control how partial failures are observed. Appropriate protocols may be enforced by substructural types.

*NOTE:* Physical resources are almost always location specific, and must be manipulated from the right location. Accessing the location can potentially be conflated with acquisition of the resource. 

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

Value sealing is a simple technique with very wide applications. The setup is simple. We have two capabilities - a 'sealer' and the corresponding 'unsealer'. At runtime, these may be allocated as a pair, using a secure uniqueness source (see above). The recommended representation:

        {:u} :: a → Sealed u a        `:` for seal
        {.u} :: Sealed u a → a        `.` for unseal

In some cases, when sealed values are sent to untrusted contexts, sealers may guide automatic use of symmetric or asymmetric encryption. However, sealers are usually implemented by trivial wrapping of the value. Value sealing has no observable impact on behavior of a correct program. Often, sealers can be completely eliminated by a compiler. 

Developers cannot observe, compare, or operate upon a sealed value without first unsealing it. But a few whole-value operations - e.g. data shuffling, copy and drop, quotation, communication - are permitted, assuming the same operation is also permitted on the underlying value type.

Developers can reason about sealed values by reasoning about distribution of sealers and unsealers. This is potentially useful for:

* representation independence and implementation hiding 
* enforce parametricity for distrusted data plumbing services
* integrity, confidentiality, authentication, rights amplification

and [more](http://erights.org/elib/capability/ode/ode-capabilities.html#rights-amp).

NOTE: In addition to unique sealers, a high level language (like AO) might support direct expression of discretionary sealers, e.g. to model abstract data types, newtypes, or modules. These might use an insecure value such as `{:foo}`. To protect against untrusted foreign code in open systems, it is also reasonable to have a variation that rewrites for each application instance, e.g. `{:$myFoo}`. 

NOTE: if serialized, sealed values might be represented as encrypted capability text. This won't apply to low-security discretionary sealers, but it can apply to instance specific or runtime generated sealers. Prefix `$` is tentatively reserved for this purpose. 

### ABC Paragraphs

ABC encourages an informal notion of "paragraphs" at least in a streaming context. A paragraph separates a batch of code, serving as a soft indicator of "this is a good point for incremental processing". A well-behaved ABC stream should provide relatively small paragraphs (up to a few kilobytes), and a well-behaved ABC stream processor should respect paragraphs up to some reasonable maximum size (e.g. 64kB) and heuristically prefer to process a whole number of paragraphs at a time. The batch would be typechecked, JIT compiled, then (ideally) applied atomically. 

A paragraph is expressed by simply including a full, blank line within ABC code. I.e. LF LF in the toplevel stream outside of any block. This corresponds nicely to a paragraph in a text file. Formally, the space between paragraphs just means identity. Paragraphs are discretionary. The reason to respect them is that they're advantageous to everyone involved, i.e. for performance and reasoning.

### Secure Hash Sources for Code Reuse and Separate Compilation

It is not uncommon in a project or service to reuse large, specialized software elements: frameworks, templates, plugins, widgets, diagrams, texts, tables, images, agents, codecs, and other software components. In an ABC stream, the most direct way to reuse code is to repeat it in the stream. Unfortunately, reuse by repetition is inefficient for bandwidth and storage. 

An alternative to repeating code is to name it. Then we can reuse large code by repeating the much shorter name. Unfortunately, most naming systems have properties that repeating code does not: collisions, potential cycles, location dependence, update and version consistency issues. These features are troublesome for security, safety, and distribution. Fortunately, we can address these issues by a more rigorous naming system. Instead of allowing humans pick names, we leverage a [secure hash function](http://en.wikipedia.org/wiki/Cryptographic_hash_function) of the content. Collisions and cycles are effectively eliminated. The 'update' and 'reuse' and 'location' concerns are cleanly separated.

ABC leverages its effects model to access these `{#secureHash}` sources. 

Here, 'secureHash' will (most likely) be SHA3-384 of an ABC subprogram, encoded as 64 octets in base64url (`A-Z` `a-z` `0-9` `-_`). When `{#secureHash}` is encountered in the ABC stream, we obtain the associated resource, validate it against the hash, validate it as an independent ABC subprogram (e.g. blocks balanced; text terminates; computable type), then essentially inline the subprogram. These sources may be 'deep', referencing more `{#secureHash}` sources.

To obtain sources, we search local cache or query proxy services, using the hash as an identifier. In many contexts, the sender is an implicit proxy; annotations in a stream may suggest extra proxies to search. To mitigate latency concerns for deep sources, a proxy is free to send a few extra sources that it anticipates will soon be required. 

Frequently used sources can sometimes be cached together with precompiled forms for performance. Thus, `{#secureHash}` sources serve as a simple foundation for separate compilation and linking in ABC. Unlike traditional shared object and linker models, ABC's design works effectively in context of secure, streamable code.

## Awelon Bytecode Deflated (ABCD)

I plan to develop a larger bytecode above ABC: ABC Deflated, or ABCD.

ABCD extends ABC with a dictionary that maps unused UTF-8 characters (U+00C0 and above, reserving lower codes) to common, correct, widely used sequences of ABC. An ABC runtime will be expected to have a database of these definitions, and perhaps even have specialized optimizations for them. ABC streams may then be compressed against this dictionary, or alternatively generated using these operators directly. 

But compression isn't the only desired characteristic. A carefully developed ABCD dictionary should capture many known-safe, obviously correct patterns with high level equational laws to simplify static analysis and useful optimizations. For example, we may introduce operators to map or fold over lists, and operators for linear algebras and matrix manipulation.

Development of ABCD shall be incremental and empirical, driven by actual data, with attention to newly popular data structures and patterns. Valid concerns include that we should not grow the dictionary too large, and we should not assign operators that might later be deprecated or proven incorrect. UTF-8 can support more than a million elements, but I don't expect ABCD will grow much beyond the two-octet UTF-8 space. 

ABCD is intended to be used together with `{#secureHash}` sources. 

ABCD is suitable for relatively short, frequent, widely used functions. Sources are suitable for large, project-specific components, templates, configurations, big but slow-changing data, and web apps. ABCD functions should be formally 'correct' because we're freezing them into the language. Sources aren't so constrained; they are easily deprecated and replaced by typical caching models.

Between these two features, ABC can be minimal without concern for performance or parsimony.

*NOTE:* Byte-stream compression below ABC is also feasible at the transport and storage layers, and can mitigate slow development and adoption of the other techniques. I am contemplating [LZHAM](https://code.google.com/p/lzham/) for this purpose.

## Ambiguous Awelon Bytecode (AMBC)

Ambiguous ABC (AMBC), is an extension to ABC to directly express AO's ambiguity feature. AMBC serves primarily as an intermediate language for AO.

AMBC extends ABC with with `(|)` characters, which represents a choice of subprograms separated by the `|` bar. E.g. `vr(wl|>M)c` has two potential meanings - `vrwlc` (swap) or `vr>Mc` (sort2). The resolution of this ambiguous meaning is not deterministic, but is constrained by typeful context (i.e. meanings with obvious type errors or assertion failures should be eliminated) and may further be guided by heuristics (i.e. weighted preferences or probabilities via annotations). 

Ambiguity is a potential asset for rapid prototyping, exploratory programming, adaptive code, and incremental refinement. Developers can represent a very large *space* of programs or data structures in a small volume of code, and may further implicitly constrain this space by use of types and assertions. This space can feasibly be explored by a number of mechanisms - i.e. satisfiability solvers, genetic programming, iterative hill climbing. 

There is a tradeoff. Resolution is expensive. It is difficult to correctly resolve effectful meanings in a streaming context. Equational reasoning, and reasoning in general, are hindered.

These weaknesses can be mitigated. In a typical AO context, resolution is at compile-time, and thus search can become a dialog with the developer. A good IDE can expose active choices to the developer and encourage refactoring of stable subprograms into non-ambiguous components. Average expense could be further reduced by application of machine learning to efficiently identify good meanings in context. In a streaming context, one might resolve for a group of paragraphs at a time, or favor an effects model that delays commitment.

AMBC can be used together with ABCD or `{#secureHash}` sources.

