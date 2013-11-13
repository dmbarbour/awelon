# Awelon Bytecode

Awelon Bytecode (ABC) is a primary component of the Awelon project. ABC is a streamable, securable, type-safe, tacit concatenative, non-strict, causally commutative, spatially idempotent, weakly legible, functional bytecode. Breaking this down:

* **streamable** supports well-behaved incremental processing
* **securable** effects via invocation of unforgeable capability text
* **typesafe** types can enforce many safety properties, and can be inferred
* **tacit** no local variable or parameter names; ops appy to environment
* **concatenative** juxtaposition is composition; to addend is to edit
* **non-strict** no commitment to common evaluation orders; temporal control
* **causally commutative** effect ordering is expressed by argument threading
* **spatially idempotent** duplicate expression doesn't duplicate effect
* **weakly legible** visible, formattable code; readable text and numbers
* **functional** higher order expressions, immutable values, pure by default
* **bytecode** UTF-8 for text, but ABC codes within Latin-1 character set

ABC is suitable for functional, procedural, and reactive programming. ABC is primarily designed for reactive demand programming (RDP). ABC can be interpreted, but is intended as an intermediate language to be compiled to native code (or LLVM, etc.) for execution. 

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

The ABC stream transforms a value within an implicit environment. ABC's basic type system isn't anything special - numbers, blocks, pairs, sums, unit and void. ABC's number type is arbitrary precision rationals.

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

*NOTE:* ABC doesn't assume any particular type inference algorithm. AO environments are free to try multiple algorithms or strategies to analyze code. ABC does assume some features for a good analysis:

* partial evaluation of static structure, as much as feasible
* relatively precise analysis similar to dependent types
* recognize common data structures - text, tables, trees, zippers
* recognize common iterative algorithms - folds, traversals, searches

There is no syntax in ABC for declaring types, but it is possible to assert conditions on the code. In essense, ABC is designed to support static type safety without ever defining 'type'. 

Pattern recognition features aren't essential, but could result in much greater efficiency for analyzing real bodies of code, and lead to more effective error messages and optimizations for runtime behaviors. For example, we might recognize operations over lists subject to loop fusion, or that a particular association list is never used in an order-dependent manner.

### Effects

Primitive ABC operators are purely functional transforms. However, ABC supports effects by invoking environment-defined operators. This invocation is expressed as `{foo}`. This calls into the environment with the text 'foo' and the full tacit value. The environment may cause some effect and return an updated value.

These invocations can also be used for annotations, or for referencing external ABC code by secure hash. Such conventions are described much later in the document.

*NOTE:* Nesting, and even appearance thereof, is not allowed in capability text. The character `{` is not allowed within capability text, and the first `}` will end the capability text.

### Capabilities

In expression `{foo}`, the text 'foo' is called *capability text*. In practice, this text should be cryptographically secure. Secure random GUIDs, encrypted text, or signed text (HMAC, PKI) are all possibilities. 

ABC is designed for [capability-based security](http://en.wikipedia.org/wiki/Capability-based_security). Capability-based security conflates controlling access with controlling distribution of unforgeable values called "capabilities". A capability can be modeled as a block containing a capability invocation, i.e. `[{foo}]`. 

Capability text is unforgeable from within ABC: there are no primitives that may invoke text as capability text. Blocks are opaque in ABC: there are no primitives that can compare blocks or convert a block to text. An environment may provide introspective and reflective capabilities that can violate these assumptions, but there is no good reason to grant such capabilities to untrusted code.

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

ABC is designed primarily for reactive demand programming (RDP), which has both spatial idempotence and causal commutativity universally. ABC optimizers and refactoring tools may assume these properties even when ABC is not targeting an RDP behavior. 

Fortunately, it is not difficult to thread a linear value through imperative code, and thereby model threads. Between substructural types and capability security, ABC can typefully enforce a procedural paradigm and many others. Interestingly, programmers can be much more precise about threading and synchronization within ABC than they can be in most imperative languages. 

### Fast and Loose Reasoning

ABC favors a philosophy of 'fast and loose reasoning' about termination properties. (cf. [Fast and Loose Reasoning is Morally Correct](http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html), Danielsson, Hughes, et al. POPL 2006.) The idea is that we should reason about programs as if every subprogram terminates or is intended to do so. This assumption simplifies loop fusion, parallelization, partial evaluation, equational laws and rewriting, non-strict evaluation, and similar. 

To help encourage fast and loose reasoning, ABC compilers should perform termination analysis, albeit accepting the possibility that the analysis fails after a limited effort:

* if termination proven, compiler is silent
* if non-termination proven, compiler raises error
* if no proof achieved, compiler issues warning

A guarantee of termination is primarily a mechanism to catch errors in reasoning. A termination analysis should catch many obviously non-terminating expressions, such as `[^$]^$`, especially focusing on common errors in reasoning. A warning may serve as a reminder that every loop should come with a guarantee of termination. 

Even with such a guarantee, it is easy to express algorithms that take more time and resources than anyone is willing to grant. In practice, developers should be seeking even stricter properties than termination, such as understanding how many resources a program requires.

### Non-Strict Evaluation

Between causal commutativity and fast and loose reasoning, ABC should not be considered strict or eagerly evaluated. An optimizer has a great deal of freedom regarding the order of evaluation. Even code with side-effects is non-strict in ABC, though it must be evaluated in a well-timed manner.

Programmers may suggest laziness, strictness, or parallelism via annotations. But such advise is discretionary. Programmers must never *depend* on lazy semantics. I.e. instead of infinite lists, use a block to model an incremental stream generator.

### Logically Timed

ABC tracks logical latency properties for atomic values (numbers, blocks). Logical latency is a rational number, indicating a time in seconds. Blocks may also have latency constraints on when they can be invoked. Logical latency is only increased by a logical delay operator. Logical delay simply increments logical latency. 

The relationship between logical latency and real-time is maintained by a scheduler. A good scheduler will keep logical and real time tightly aligned with predictable failure modes, using both soft and hard mechanisms, and some scheduling may occur at compile time. If an effect is invoked on the future, it may be scheduled without invoking it immediately while computation continues elsewhere. Or if a computation is running ahead of where it needs to be, the scheduler may devote more resources to other computations.

The logical model of time, especially on a real timeline (seconds, not arbitrary units), is valuable for understanding and controlling feedback behaviors, for achieving consistent behavior for reactive networks overlays, for comprehending interaction of concurrent effects. However, developers don't always need to think about time. In many cases, the role of assigning temporal properties can be pushed into other layers - networking, effects, frameworks.

Related: [Computing Needs Time](http://www.eecs.berkeley.edu/Pubs/TechRpts/2009/EECS-2009-30.pdf), Edward Lee 2009.

### Data is Code

Structured data in ABC systems is modeled as a stream of ABC code that constructs it. For example, rather than a pair using a structured syntax `(42,108)` we might send code that builds a pair from a unit value: `#108#42lc`. 

This approach offers powerful advantages for simplicity, self-validation, compression, and procedural generation. Further, it is much more extensible. To addend the code is to edit the data. The code can be used as history, and formally refactored and optimized for time or space performance. Lenses and views are readily applied. 

For connectivity between Awelon project systems, data will uniformly be serialized in this manner. Additionally, a communication context - a value that belongs to the connection - will be maintained at each endpoint, enabling more optimal communication by storing some reusable macros or templates. 

## ABC Behavior Details

This section has a more detailed description of the ABC behaviors.

Note that most ABC operators have a `* e` term on the right, indicating that the operation ignores a certain amount of structure. The goal here is to avoid using blocks for every little thing.

### Data Plumbing

ABC provides a minimal set of primitive operators for block-free structure manipulation. Below, `(a * b)` denotes a product (pair), and `1` denotes unit, identity for products, which discussed later.

        l :: (a * (b * c)) -> ((a * b) * c)
        r :: ((a * b) * c) -> (a * (b * c))
        w :: (a * (b * c)) -> (b * (a * c))
        z :: ((a * b) * (c * d)) -> ((a * c) * (b * d))
        v :: a -> (a * 1)
        c :: (a * 1) -> a

There are many potential "minimal sets" of data plumbing primitives. ABC's particular set is aiming for some simple symmetries and optimizations. Here, `lzrw` is sufficient to encode any linear manipulations of a structure where the rightmost element remains in place. `v` and `c` are non-linear manipulations (they add and remove structure) and enable motion of the rightmost element.

Example encodings:
        
        lzrw :: (a * (b * (c * d))) -> (c * (a * (b * d))) -- rot3
        vrwlc :: (a * b) -> (b * a) -- primSwap

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

After construction, a block can be applied by the `$` operator:

        $ :: [x->x'] * (x * e) -> (x' * e)

Blocks in ABC form a basis for secure reasoning, loop behaviors, and higher order programming. Loops are modeled by fixpoint combinators that repeatedly copy and apply a block, e.g. `[^$]^$` is the equivalent to the lambda calculus `(λx.(x x) λx.(x x))`. Higher order programming is achieved simply by separating construction of the block from the point of application.

For security, blocks offer three significant properties:

* `$` hides part of the tacit environment from the block.
* blocks are opaque; they may encapsulate information and authority.
* blocks have substructure; they can typefully enforce contracts.

Thus, blocks enable information hiding on behalf of both the user and the provider. A block can be said to 'encapsulate authority' if it contains capability text, since ABC provides no operators to forge capability text. Substructural types are discussed later.

### Numbers

ABC's built-in number type is arbitrary precision rationals. (In some cases - when the required range and precision are known - a compiler may substitute use of integers, fixpoint, or floating point numbers.) ABC has no number literals. `#42` is technically a sequence of three ABC operators. The operator `#` introduces the number 0 into the environment, and each decimal digit (0-9) means "multiply by ten and add this digit's value".

        # :: e -> N(0) * e
        0 :: N(x) * e -> N(10x+0) * e
        1 :: N(x) * e -> N(10x+1) * e
        2 :: N(x) * e -> N(10x+2) * e
        ...
        9 :: N(x) * e -> N(10x+9) * e

Thus natural numbers are thus expressed as if by literal. But rational or negative numbers must be represented as a computation that generates them. ABC provides only a few elementary, scalar mathematical operators: add, multiply, and additive or multiplicative inverses. It also provides a divmod operator to help infer precision and modulus information:

        + :: (N(a) * (N(b) * e)) -> (N(a+b) * e)
        * :: (N(a) * (N(b) * e)) -> (N(a*b) * e)
        - :: (N(a) * e) -> (N(0-a) * e)
        / :: (N(non-zero a) * e) -> (N(1/a) * e)
        Q :: (N(non-zero b) * (N(a) * e)) -> (N(r) * (N(q) * e))
            such that q integral, r in (b,0] or [0,b), and qb+r = a

A few example numbers might be:

        #2#3/*-  (-2/3)
        #123/00/ (1.23)

Direct expression of rational numbers, or very large or small numbers isn't very compact in ABC. Developers should consider use exponential representations of such numbers to start with (such that adding is multiplication, like decibels). A language built above ABC, such as AO, can provide a more traditional and compact syntax for numbers. 

ABC is not rich in math. To model a square root or trigonometric function would likely require iterative computation with tolerances. Irrational numbers might be modeled as an incremental stream that generates digits.

*BACKGROUND:* Floating point numbers were rejected due to how difficult it is to ensure deterministic semantics for floating point across implementations, comparisons, equational reasoning. However, high performance graphical or scientific computing environments should provide, via capabilities, vectors and matrices of floats, and access to GPGPU computing.

### Text Literals

ABC has a built-in support for representing unicode text as a literal. Literal text is expressed in a block format, encoded in UTF-8 as part of the normal ABC stream:
        
        "text has a block format 
         it starts with double quote
         it may continue on multiple lines
         each ending with linefeed (10)
         and each starting with a space
         terminate with tilde (126)
        ~

This block structure eliminates need for escapes, except for LF which is escaped by the space. The LF preceding `~` is dropped. If anything other than space or `~` follows LF, the ABC stream is in error. ABC text is capable of quoting ABC code without being too ugly. By convention, text typically starts a new line. 

Sadly, text isn't very aesthetically pleasing for a single word or line:

        "Text
        ~

Of course, being pretty is not a primary goal of ABC. Keeping text and numbers visible helps with debugging, but to really read ABC one should disassemble the ABC stream against an AO dictionary and present it in a dedicated UI.

Text is not a distinct type for ABC. Rather, text is a compact representation for introducing a list of small integers corresponding to the Unicode codepoints. The above code, consisting of six characters, has the same meaning as `#3#116l#120l#101l#84l`, which would have the type: 

        e -> (N(84) * (N(101) * (N(120) * (N(116) * N(3)) ))) * e

The terminal `3` is arbitrary, though has some historical significance as the ETX (end of text) character in ASCII. It also serves as a weak indicator that the list was intended to be interpreted as text. 

*BACKGROUND:* Text was initially envisioned as a distinct type for ABC. However, that design traded a fair amount of simplicity (extra structure manipulation and analysis codes, extra types to track and comprehend, new kinds of code to optimize) for rather dubious performance benefits. In practice, a streaming ABC interpreter can store text in a compact form, even recognize text when forming tuples. Further, common list-processing operations, such as reverse or addend could be recognized by simple pattern-matching and swapped for a highly optimized variant. (A compiler, having more time to crunc code, should do even better.)


### Substructural Reasoning

[Substructural types](http://en.wikipedia.org/wiki/Substructural_type_system) are interesting because they allow expression of structured behavior (dataflow and control flow) without relying on a structured syntax. For example, one can require a handshake complete, or that a promise be resolved, or that a callback be performed. 

In ABC, only blocks can have substructural types. This is represented by marking an existing block as relevant, affine, or both (called linear):

        k :: ([x->y] * e) -> ([x->y]' * e) (relevant, no drop)
        f :: ([x->y] * e) -> ([x->y]' * e) (affine, no copy)

An affine block is no longer subject to the copy `^` operator. A relevant block is no longer subject to the drop `%` or conditional apply `?` operators. A relevant or linear block may still be applied with `$`, which removes the block. Adding substructural attributes to a block is idempotent and commutative.

When two blocks are composed, the substructural attributes are inherited:

        [code]k [more]f o = [codemore]kf
        [code]f [more]  o = [codemore]f

A quotation inherits the substructural attributes of every blocks it quotes:

        [code]k[more]fl' = [[code]k[more]flc]kf
        [code]f' = [[code]fc]f

Awelon project leans heavily on substructural types, e.g. for exclusive bindings to state resources, unique identity, sealer/unsealer pairs, or enforcing threaded behaviors for imperative processes. Potentially, relevant types could model obligations, and affine types could model limited resources. Idiomatically, a linear block will often return a new (possibly updated) linear block upon application.

*NOTE:* If a relevant block is copied, both copies are relevant. I've contemplated an alternative, that one of the two copies is not relevant. But I decided against it because it is difficult to explain for deep structure.

### Conditional Behavior

ABC uses sum types `(a + b)` as the foundation for conditional behavior. A sum type represents that we're either in the left condition with type `a` or in the right condition with type `b`. A boolean can be modeled as type `(1 + 1)`, and an optional with type `(a + 1)`. Type `0` describes void, identity for sums, discussed later.

Conditional behavior is modeled with the `?` operator:

        ? :: (Droppable b) => b@[x->x'] * ((x + y) * e) -> (x' + y) * e

To apply behaviors for other conditions, one must use data plumbing to shift the desired condition to the top. Sum types use their own set of data plumbing operators:

        L :: (a + (b + c)) * e -> ((a + b) + c) * e
        R :: ((a + b) + c) * e -> (a + (b + c)) * e
        W :: (a + (b + c)) * e -> (b + (a + c)) * e
        Z :: ((a + b) + (c + d)) * e -> ((a + c) + (b + d)) * e
        V :: a * e -> (a + 0) * e
        C :: (a + 0) * e -> a * e

*ASIDE:* Sums are processed as one element of a larger product, rather than directly mirroring the product operations with `L :: (a + (b + c)) -> ((a + b) + c)` and so on. The motivation is to diminish need for block composition and quotation in the common case. 

Sum types have several nice qualities. They separate the observation of a condition from action upon it. They remain open for extension and composition. Like products, sums have a simple and uniform structure that admits refactoring of data plumbing. There is no ambiguity for labeling of conditions. In a partitioned environment (CPU vs. GPU values, client vs. server), ABC can typefully control dataflows across a sum. 

To construct sums, developers can compare or introspect values. 









TODO: distribution, conjoin, disjoin, perhaps static vs. dynamic.
TODO: conditional (e.g. less, equal) operations that produce sum in first place.

There is no comparison for unit. (Unit does not have comparison properties.) Might still want static conditions vs. dynamic ones. (Static has advantages for distrib, etc.)

### Unit and Void

Unit and void are special types in ABC. Unit (type `1`) is identity for the product type, and Void (type `0`) is identity for the sum type. The principle idea of unit and void is that they introduce structure *without adding information*. I.e. `(a * 1)` has just as much information as `a`, but now it's wrapped in a structure. 

If we aren't careful, we'll have a problem: structure itself can carry information. I.e. `1`, `(1 * 1)`, `(1 * (1 * 1))`, and `((1 * 1) * 1)` are four obviously distinct structures, and distinction can carry information. Fortunately, it's fine that this distinction exists in a meta-layer, so long as it is not observable from *within* the ABC program.

This feature is achieved in a simple way.

For unit, we forbid introspection. If you query whether `(1 * 1)` is a pair, you get an affirmative. However, if you ask whether `1` is a pair, you get a type error. Thus, the conditional information is observable only in the static or compile-time layer. This property is quite useful: unit now serves as a simple barrier against dynamic introspection and reflection, and enables developers to express data structures where sizes must be known statically (e.g. matrices, program stacks). 

For void, there is no actionable query. Void is a vacuous, dead branch. Operations on void will always typecheck unless they're internally inconsistent. (It is recommended that an ABC typechecker validate actions on void for consistency.) Since `0` can represent any type, we cannot gain information from introspecting `0`. It might represent a sum, a product, a number, or a block. Of course, if we introspect `0`, we are implicitly asserting that it does not represent unit (with respect to consistency checks). Regardless of what void represents, it's still a dead branch so we cannot act on it.

Thus, unit and void provide structure without information. Unit is useful for providing static structure and a barrier against introspection. Void is useful for manipulating proofs, consistency checks, and typechecks for code we never plan to run.

### Assertions

Assert we *aren't* in a branch (never that we *are* in one)

* track potential ranges for numbers, e.g. protect against divide-by-zero


### Temporal Reasoning
* location and latency properties model where and when values can be accessed
* latency constraints for blocks and sealed values - expires, ripens

Absolute latency should never be observable. But maybe can compute difference of latency difference between two values. OTOH, computing latency difference in a dynamic scenario would logically require waiting. So maybe computing difference in latency doubles as a synch operation? That could be useful.

 Should 'synch' be primitive? Not so sure... maybe? I want latencies to be easy to reason about, including equality of latencies. 

### Spatial Reasoning

        $, ', +, *, Q... multi-parameter ops


* location and latency properties model where and when values can be accessed

Location values are not observable, except by capability.

### Sealed Values

Sealed values are a rather odd concept in ABC, as they cannot actually be constructed. Rather


* sealed value types model information-hiding and rights amplification







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
* Thus total encode (including capability wrapping) is:
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
* provide hints for proving safety or termination
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

For copying or dropping capabilities, it seems wise to give the environment a decision on how to implement the copy or drop operation. 

### Incremental Processes

Every loop expressed in ABC should terminate. A compiler is allowed to reject a program if it proves non-termination, and may complain if it cannot prove termination. The traditional `while(true)` process loops of imperative programming are not allowed. This is a good thing! Those `while(true)` loops have *awful* properties for composition, extension, and reuse.

ABC systems instead model long-running behaviors more explicitly:

* RDP behaviors
* streaming ABC
* incremental processes

ABC is designed primarily for RDP, and Awelon project makes heavy use of unbounded streaming ABC for serialization and environment manipulation. But if ABC used for imperative processes, an incremental process model should be favored. In that case, each process might have a structure similar to:

        type Process a b = a -> (b, Process a b)

At each step, a process will perform a finite (usually small) incremental amount of work. These processes can be composed sequentially or in parallel, or model ad-hoc workflows. These processes can model environments, encapsulating more processes and modeling connectivity or dispatch. The process graph can be dynamic, changing from step to step, represented by returning the process for the next step. Large, specialized processes are constructed from smaller, reusable ones. Process control is implicit with simple behavior: we can pause between steps. 

In ABC, this model of incremental processes can generally be parallelized, fused, and optimized to a very high degree (based on causal commutativity, fast and loose reasoning). 

#### (Thought:) Promise Pipelining and Lattices

My intuition is that incremental process model could be greatly augmented with [promise pipelining](http://en.wikipedia.org/wiki/Futures_and_promises#Promise_pipelining). The process could actually run a few steps in advance of the effects, computing the dataflow before the data is fully computed. 

The motivation for this would be to tighten up latency and well-timing properties, and perhaps gain some efficiency via batching.

Interestingly, we could also use lattice based promises, such that we can observe an 'incremental' (but not necessarily 'final') value. This technique could result in an imperative system that is very robust even when timing falls slightly behind. (cf. Lindsey Kuper's [LVars](http://lambda-the-ultimate.org/node/4823))


