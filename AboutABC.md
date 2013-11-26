# Awelon Bytecode

Awelon Bytecode (ABC) is a primary component of the Awelon project. ABC is a streamable, securable, type-safe, tacit concatenative, non-strict, causally commutative, spatially idempotent, weakly legible, functional bytecode. Breaking this down:

* **streamable** supports incremental processing; no sections or addressing
* **securable** effects via invocation of unforgeable capability text
* **typesafe** types can enforce many safety properties, and can be inferred
* **tacit** no local variable or parameter names; ops appy to environment
* **concatenative** juxtaposition is composition; also trivial to decompose
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

### ABC Paragraphs

ABC encourages an informal notion of "paragraphs". A paragraph separates a batch of code, serving as a soft, discretionary indicator of "this is a good point for incremental processing". A well-behaved ABC stream should provide relatively small paragraphs (up to a few kilobytes), and a well-behaved ABC stream processor should process a whole number of paragraphs in each step.

A paragraph is expressed by simply including a full, blank line within ABC code. I.e. LF LF in the stream. This corresponds well to a paragraph in any English text file.

Paragraphs may be enforced in serialization layers - simply disconnect from a client that doesn't provide timely paragraphs of a reasonable size (perhaps with soft, probabilistic tolerance). Paragraphs are convenient as an implicit boundary for batching, typechecking, compilation, and atomic update, while still enabling flexibility to process multiple batches together for performance reasons.

Formally, the space between paragraphs just means identity. Paragraphs are not preserved within blocks, and use of paragraphs is ultimately discretionary within the toplevel stream.

## ABC Behavior Overview

The ABC stream contains operators, literals (blocks, text, numbers), and invocations.

### Operators

Each operator is represented as a single character, denoting a function. For example, operator `r` denotes the right association function, with type `((a * b) * c) → (a * (b * c))`, and `w` denotes the stack swap operator `(a * (b * c)) → (b * (a * c))`. For any version of ABC, there is a fixed set of operators.

ABC operators are applied in sequence to an implicit value external to the ABC stream. This value is typically a composite, constructed of `(a * b)` pairs. This value may represent documents, a stack-based computing environment, or many things. 

ABC operators manipulate a small part of this structure using a stack-like metaphor whereby `(a * (b * (c * ...)))` represents a stack with top three elements `a`, `b`, and `c`. Deep manipulations are modeled by shuffling relevant elements to the top of the stack, manipulating them, then shuffling them back to their proper location. (Idiomatically, ABC paragraphs should start and end with elements in proper location.) *NOTE:* data shuffling can become expensive if ABC is interpreted, but a great deal can be eliminated if ABC is compiled.

Developers can legitimately comprehend juxtaposition as functional composition. The ABC sequence `rw` has type `((a * b) * c) → (b * (a * c))`. Conversely, an ABC program can easily be decomposed into valid subprograms and software components.

### Literals

Blocks and text are the literals of ABC. They require special attention by the ABC reader and result in values added to the stack. Numbers are a pseudo-literal. Literals can be understood as functions that introduce a value.

A block contains a finite sequence of ABC code, and may be understood as a first-class function that simply introduces its value. E.g. `[rw]` has type `x -> [((a * b) * c) → (b * (a * c))] * x`. Blocks are the foundation for loops, conditional behavior, and higher order programming. Blocks also support security, protecting interests of both provider (by encapsulating information or authority) and user (by constraining access on apply: `$ :: [x→x']*(x*e)→(x'*e)`).

Numbers use operator `# :: e → (N(0)*e)` to introduce a new zero, then each digit `0-9` has meaning of the form `3 :: N(x)*e → N(10x+3)*e`. Thus, numbers aren't literals, but natural numbers such as `#123` are close enough for legibility. Rational numbers are produced through operations on natural numbers, e.g. `#2#3/*` is two thirds.

Text is shorthand for producing a list of small numbers between 0 and 1114111, the Unicode codepoints. *NOTE:* ABC has no support for binaries, but text can carry base-64 with reasonable efficiency.

### Invocations

Effects in ABC are achieved by invoking environment-provided operators: `{foo}` would invoke the environment with token "foo" and the tacit argument. This token is unforgeable from within ABC: there are no operators to invoke computed text as a capability. Access to effects is typically granted through a block, of a form like `[{foo}]`. 

For open or distributed systems, the token should be cryptographically secure and specific to an environment - e.g. encrypted text, signed text, or secure random GUID - such that access to effects is [securable](http://en.wikipedia.org/wiki/Capability-based_security) in presence of ad-hoc code distribution.

Environment-provided operators may be first-class. For example, `[{obj:SecureRandomGUID}]` might serve as a reference to a specific object in the environment. Applying this block would essentially result in passing a message to the object.

(*Note:* no nesting! These tokens may not contain `{` or `}` characters.)

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
        ' :: x * e → [1→x] * e
            #42' = [#42c]
            [vrwlc]' = [[vrwlc]c]

After construction, a block is applied with the `$` operator:

        $ :: [x→x'] * (x * e) → (x' * e)

Loops can be modeled as fixpoint combinators that copy and apply a block, e.g. `[^$]&$` is a non-terminating loop, equivalent to the lambda calculus `(λx.(x x) λx.(x x))`. Of course, a proper loop should have a halting condition.

In ABC, higher order programming can be modeled as a block that expects a block as an argument. Currying (partial application) can be modeled in terms of quotation and composition.

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

ABC is not rich in math, nor especially efficient at it. High performance graphical or scientific computing will often be handled indirectly, with support of external resources. Rich math is better modeled symbolically than directly evaluated.

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

Text is not a distinct type for ABC. Rather, text is understood as a compact representation for introducing a list of small integers (range 0..1114111). The above text essentially means `#3#116l#120l#101l#84l`, which would have the type: 

        e → (N(84) * (N(101) * (N(120) * (N(116) * N(3)) ))) * e

The terminal `3` for a list of text is arbitrary, chosen for its meaning as ETX (end text) in C0. By convention, ABC systems use list terminators as type indicators to support visualization and dependent typing.

### Identity

ABC treats two whitespaces - SP (32) and LF (10) - as identity operators with type `x → x`. Effectively, whitespace in the ABC stream may be ignored. Also, the empty program is equivalent to identity, since it performs no operations on the tacit input.

(Tabs or carriage returns are not valid ABC operators. If encountered in an ABC system, an error must be raised, as for any other invalid operator.)

### Substructure

[Substructural type](http://en.wikipedia.org/wiki/Substructural_type_system) are interesting because they can express structured control-flow or dataflow orthogonally to syntax. They can enforce that protocols and handshakes complete, that resources are released, that promises are kept, that callbacks or continuations execute once and only once. Substructure is also useful for modeling uniqueness or ensuring exclusive write access to a resource.

In ABC, only blocks have substructural type. This is represented by marking an existing block with substructural type:

        k :: ([x→y] * e) → ([x→y]' * e) -- relevant, no drop
        f :: ([x→y] * e) → ([x→y]' * e) -- affine, no copy

These operations are naturally idempotent and commutative.

An affine block may not be copied. A relevant block may not be dropped. A linear block may not be copied or dropped. However, a relevant or linear block may still be applied with `$`.

When blocks are quoted or composed, or a structure containing blocks is quoted, the result inherits substructural attributes of all the components: 

        [code]f [more]k o = [codemore]kf
        [code]f [more]  o = [codemore]f
        [code]k [more]fl' = [[code]k[more]flc]kf
        [code]f' = [[code]fc]f

When a relevant block is copied, both copies are relevant. (*Note:* Technically, only one of the two copies must be relevant. However, it is difficult to explain this in a type system, and difficult to track in a streaming scenario. For simplicity, both copies are relevant.)

### Conditional Behavior

A sum type, `(a + b)`, represents that we're either right with `b` or left with `a`. A sum type is constructed by observing a condition. In ABC, primitive operators enable observing a few basic conditions, by convention punning 'right' with 'true', i.e. `(false + true)`. Observation primitives include:

        P :: (Observable x) ⇒ x * e → (x+x(a*b)) * e -- x is pair
        S :: (Observable x) ⇒ x * e → (x+x(a+b)) * e -- x is sum
        B :: (Observable x) ⇒ x * e → (x+x([a→b])) * e -- x is block
        N :: (Observable x) ⇒ x * e → (x+x(N(a))) * e -- x is number
        > :: (Comparable x y) ⇒ x * (y * e) → ((y*x)+(x*y)) * e -- y > x
            #4 #2 > -- observes 4 > 2. Returns (N(2)*N(4)) on right.

Most types are observable and comparable. Pairs are greater than numbers, and numbers are greater than sums. Pairs compare first before second, i.e. such that text or lists will compare in lexicographic order. Sums treat left as before right, and only compare inner elements if the branching is equal. 

However, blocks are not comparable, unit is not observable, and unit may be compared only with unit (and is equal). Unit provides a foundation for static structure: in general, developers must know statically where to find unit values. This is discussed later.

After a condition is observed, we can conditionally apply a block:

        ? :: (Droppable b) ⇒ b@[x→x'] * ((x + y) * e) → (x' + y) * e

A block with the 'relevant' substructural property cannot be applied in this manner. Any such block must be applied under all conditions.

Sums have their own set of data shuffling operators:

        L :: (a + (b + c)) * e → ((a + b) + c) * e
        R :: ((a + b) + c) * e → (a + (b + c)) * e
        W :: (a + (b + c)) * e → (b + (a + c)) * e
        Z :: (a + (b + (c + d))) * e → (a + (c + (b + d))) * e
        V :: a * e → (a + 0) * e
        C :: (a + 0) * e → a * e

Type `0` is identity for sum, called 'void', and corresponds to vacuous argument. Analysis may infer a type for void to detect inconsistencies in dead code. 

We also can distribute, factor, and merge sums:

        D :: a * ((b+c) * e) → ((a*b) + (a*c)) * e -- distrib
        F :: ((a*b) + (c*d)) * e → (a+c) * ((b+d) * e) -- partial factor
        M :: (a+a') * e → a * e -- merge

Full factor is modeled by combining partial factor and merge:

        FM :: ((a*b)+(a'*c))*e → a*((b+c)*e) -- full factor; inverse of D

On merge, the types `a` and `a'` must be compatible for future operations, but they don't need to be exactly the same. What 'compatibility' requires may be judged in context. However, conservatively, any unit values in `a` and `a'` must be structurally aligned. 

Sums may also be copied or dropped (with `^` and `%`) assuming both element types may be copied and dropped.

*ASIDE:* ABC avoids a form of [boolean](http://existentialtype.wordpress.com/2011/03/15/boolean-blindness/) [blindness](http://www.hxa.name/notes/note-hxa7241-20131124T0927Z.html) where subprograms lose information about whatever was compared or observed. Developers can explicitly scrub results if they wish to reduce a comparison to a boolean `(1 + 1)`.

### Partial Functions and Contracts

ABC provides a simple operator for partiality, assertions, and contracts:

        K :: (a + b) * e → b * e

This operator represents a form of divergence: if we're in the left, that's equivalent to a type error and we'll stop the program as quickly and cleanly as possible. Otherwise we're okay. However, ABC is not restricted to runtime detection of this error. Expressing partial functions with `K` enables ABC to infer dependent types and contracts. If `K` cannot be statically proven safe, the programming environment may issue a warning or raise an error.

### Spatial-Temporal Features

Question: beyond capabilities, how is ABC involved with spatial properties? Is there some conventional model of capabilities I need for typechecking?


ABC models spatial properties in terms of logical partitions, and temporal properties in terms of relative latencies. Within a product or sum, the different elements may have different spatial-temporal attributes - for example, we can have types of the form `(Number@CPU * Number@GPU)`. In addition, the product or sum as a whole may have a location, with regards to where it can be introspected.

In general, information about spatial-temporal attributes is *privileged*. There are no ABC operators to query when or where a value is computed.

 capabilities, ABC behavior cannot depend on when or where it executes. There is no way to ask a number when or where it is computed.

modulo special capabilities, ABC code may not introspect when or where a value is computed. 

Additionally, the product or sum itself has a concept of spatial-temporal 'evidence' - i.e. regarding when and where knowledge that the product is a product becomes available. This is important, for example, when loading text on a remote machine.

The spatial properties are not directly accessible from ABC. Communicating between partitions is considered an effect, and is thus controlled by use of capabilities. 



Effects and their capability texts are specific to a partition. To be reusable across partitions, subprograms are written in a pure or capability-secure manner that does not hard-code any capability text (modulo annotations or references to ABC resources, neither of which are effectful). Of course, partial evaluation can specialize reusable programs, distributing capabilities at compile-time. 

The notion of logical partitions is very versatile:

* Heterogeneous systems can be modeled as partitions with different resources and effects. 
* Distributed systems are modeled by having some communication capabilities admit disruption.
* Staged programming can be modeled by modeling asymmetric communication between some partitions (i.e. different stages become different spatial partitions). 
* Confinement or purity can be enforced by computing a block in a fresh logical partition.


To perform conditional operations on a sum type requires projecting information *into* the condition, rather than the converse. This has important implications with respect to spatial properties, e.g. ABC can typefully enforce that a condition computed on the GPU is not accessible for decisions on the CPU. 


 Naturally, ABC code containing embedded capabilities is almost never polymorphic; reusable ABC code is either pure or explicitly models distribution of capabilities. Distributed systems are modeled in terms of communication capabilities that admit disruption or failure. 

There are no primitives to communicate between partitions.

Spatial-temporal information is considered privileged. That is, without a dedicated capability, an ABC subprogram cannot ask *where* or *when* it is running. 


As a general rule, access to spatial-temporal information is considered privileged. That is, an ABC subprogram cannot vary its behavior based on *where* or *when* it is running, unless it is explicitly granted that information or has a special capability to acquire it. 



*ASIDE:* For RDP, the convention for modeling disruption is to first model acquisition of a connection, which may fail, then to treat the connection as reliable. This separates failure handling from the acquisition code.

In a 

 The separation is useful. 

 This works well in a reactive model, since the acquisition may reactively fail or recover over time.

 Heterogeneous systems can be modeled as partitions with different resources and effects. Distributed systems can be modeled by having some communication capabilities model disruption.

 allow disruption. RDP can leverage reactivity by separating acquisition of a st

        getConnection :: something → (fail + connection)
        connection is 





There are no primitives for communication between partitions. 

ABC has no primitives for communication between partitions. That is, communication between partitions requires an explicit capability. Distributed systems are generally modeled in terms of communication capabilities that allow disruption.

every point-to-point communication requires a capability.


*NOTE:* Even pure ABC can be incompatible with physical constraints of some partitions. For example, if a partition represents a GPU shader, not every ABC program can be compiled to a valid shader. In these cases, we might accept partiality, that a compiler may reject some well-typed programs because it doesn't know how to translate them. Alternatively, we might model a DSL within ABC.

that we can prove is safe.


This is essential for RDP: a single RDP behavior can model reactive overlay and orchestration networks that interact across heterogeneous servers, clients, CPUs and GPUs. However, spatial-temporal features are useful even for imperative code as a basis for precisely reasoning about concurrent behavior, mobile code, consistency, progress, and scheduling. 

A compiler for ABC might break a holistic program into shards that maintain behavior in each partition.

I'll consider this in two parts: temporal attributes, and spatial attributes.




* location and latency properties model where and when values can be accessed
* latency constraints for blocks and sealed values - expires, ripens

Absolute latency should never be observable. But maybe can compute difference of latency difference between two values. OTOH, computing latency difference in a dynamic scenario would logically require waiting. So maybe computing difference in latency doubles as a synch operation? That could be useful.

Should 'synch' be primitive? Not so sure... maybe? I want latencies to be easy to reason about, including equality of latencies. 

multi-parameter operations
spatial properties of sums and products

Location values are not observable, except by capability.


Logical latency properties aren't just for type safety. They guide the scheduler, and support logically synchronous actions on distributed objects, which may result in synchronized behavior on hardware if the hardware supports precise buffering and timing.

 for atomic values (numbers, blocks). Logical latency is a rational number, indicating a time in seconds. Blocks may also have latency constraints on when they can be invoked. Logical latency is only increased by a logical delay operator. Logical delay simply increments logical latency. 

The relationship between logical latency and real-time is maintained by a scheduler. A good scheduler will keep logical and real time tightly aligned with predictable failure modes, using both soft and hard mechanisms, and some scheduling may occur at compile time. If an effect is invoked on the future, it may be scheduled without invoking it immediately while computation continues elsewhere. Or if a computation is running ahead of where it needs to be, the scheduler may devote more resources to other computations.

The logical model of time, especially on a real timeline (seconds, not arbitrary units), is valuable for understanding and controlling feedback behaviors, for achieving consistent behavior for reactive networks overlays, for comprehending interaction of concurrent effects. However, developers don't always need to think about time. In many cases, the role of assigning temporal properties can be pushed into other layers - networking, effects, frameworks.

Related: [Computing Needs Time](http://www.eecs.berkeley.edu/Pubs/TechRpts/2009/EECS-2009-30.pdf), Edward Lee 2009.


### Sealed Values (preliminary)

The notion of sealers, unsealers, and sealed values is very useful for modeling ADTs, encapsulation, identity, security patterns, and rights amplification. ABC is designed with the expectation that developers will leverage this pattern to address many challenges traditionally handled through module systems.

In pseduo-types:

        type Sealer U = Value → Sealed U Value
        type Unsealer U = Sealed U Value → Value
        type NewSealerUnsealer = U! → (Sealer U, Unsealer U)
            where U! describes an affine uniqueness source

 value and return a sealed value, specific to the sealer
* an unsealer will process a sealed value from the corresponding sealer

a capability that takes a value and returns a sealed value
* unsealer: a capability that takes a sealed value from the corresponding sealer, and returns the underlying value (no longer sealed).



A new sealer/unsealer pair can only be constructed through a capability that takes a unique value as an argument. (Uniqueness can be enforced by substructural types.) Anyhow, there is no primitive to create a sealer/unsealer pair, nor is there any primitive source of uniqueness. The sealer/unsealer concept requires support from the environment. 

From these, it is easy to also construct a capability that will unseal a value, apply a block to it, then seal the result up again. 

Anyhow, sealed values greatly benefit from recognition by type systems and optimizers. Especially of interest is what it might mean to seal a value that is distributed across space or time. It isn't clear, at the moment, whether any ABC primitives would help out, or whether some convention (in the naming of sealers and unsealers) would be sufficient. A variation on sealers/unsealers is to create a 'sealed space' - a new 'partition' for values which has a new set of capabilities for entry and exit. This could be useful for enforcing that a subprogram is typed to operate in any space.

I plan to return to the issue of systematically modeling sealed values at a later time.




## ABC Assumptions

This section discusses a few high level properties of ABC's design that cannot readily be inferred from the operators or tacit concatenative streamable structure.

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

Fortunately, it is possible to achieve spatial idempotence and causal commutativity even for imperative programming. These properties can be enforced via the effects model, by careful design of capabilities. Affine or linear capabilities - representing threads or objects - renders spatial idempotence irrelevant (since you can't duplicate the expression). Causal commutativity results in minimal, promise-like synchronization with a natural DAG. If necessary, race condition indeterminism can be modeled by [oracle machine](http://en.wikipedia.org/wiki/Oracle_machine) capabilities.

Spatial idempotence and causal commutativity offer valuable properties for equational reasoning and optimizations. ABC programs can be manipulated in many ways similar to pure functions.

### Fast and Loose Reasoning for Termination

ABC favors a philosophy of 'fast and loose reasoning' about termination properties. (cf. [Fast and Loose Reasoning is Morally Correct](http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html), Danielsson, Hughes, et al. POPL 2006.) The idea is that - with respect to optimizations, equational laws, rewriting, loop fusion, parallelization, laziness - we should assume every subprogram terminates (or is intended to). 

To help enforce and remind developers of this philosophy, ABC compilers should perform termination analysis, and long-running programs should be modeled as RDP behaviors or incremental processes (see conventions, later).

ABC is Turing complete, so we can't expect a decision to be reached for every program. However, termination analysis is really about avoiding errors in reasoning. And *most errors aren't subtle*. Accidental non-terminations often can be detected, and many developers will write reusable software such that analysis passes without a 'could not prove' warning.

Termination is a weak property. In practice, we often wish to reason about performance characteristics: latency, memory requirements, resource usage. Ackermann function terminates, but not within reasonable bounds. However, termination is a great start.

### Flexible Safety Analysis

ABC has an implicit type system with six structural types (pair, sum, unit, void, number, block), substructural types (relevant, affine, expires), and modal types for spatial-temporal attributes (latency, location, sealed values). Termination analysis is recommended to validate fast-and-loose reasoning. ABC further supports inference of dependent types or contracts by use of operator `K`.

However, ABC does not specify any inference algorithms.

The idea, by not specifying algorithms, is to not tie ABC to any particular analysis model, and thus enable independent analysis.

By not specifying an algorithm, I hope to enable independent evolution of analysis, a growing array of recognizers and strategies to prove safe structures, algorithms, and architectures. 

The philosophy for static analysis of ABC is:

* prove it right, pass it silently
* prove it wrong, raise an error
* indecision, pass with a warning and check at runtime
* incrementally improve strategies to efficiently decide more programs

By allowing indecision, analysis of ABC can freely move between heavyweight and lightweight depending on context, whether the code is streamed or a more traditional application, and so on. 

Most static languages reject programs unless they're provably correct according to the type system. ABC's more relaxed philosophy accepts (with warning) code that is not proven wrong. I think this may give ABC a more dynamic feel, especially if combined with dependently typed structures. However, ABC is not a dynamic language. There are no ad-hoc coercions. There are no operators to avoid or recover from errors. 

I believe that ABC's philosophy is close in nature to [gradual typing](http://en.wikipedia.org/wiki/Gradual_typing), albeit with more inference and less annotation.

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

* suggest blocks be lazy or parallel
* suggest blocks be computed on GPU or FPGA
* suggest sequencing of potentially lazy value
* suggest use of memoization or caching
* suggest specialization or JIT at points in code
* provide hints for proving safety or termination
* improve blame, error, warning messages
* tracing annotations, track to original code
* assert confinement or purity of blocks
* compile-time traces, run-time debug logs
* debugger integration - breakpoints, visualization 
* report features when streaming, e.g. support for secure hashes
* estimate heap sizes, quotas, tolerances for process control

Annotations can be very useful despite their lack of formal meaning within the program. Developing a great set of annotations will take much convention and de-facto standardization.

If an ABC system doesn't understand an annotation, it should ignore it rather than raise an error. If part of an ABC processing pipeline, it should pass the annotation on unchanged since it might be meaningful in a later stage.

### Data is represented by Code

Data serialization in Awelon systems is modeled by a stream of ABC code that reconstructs the data. E.g. a pair `(42,108)` might be serialized to `#108#42lc`. In general, this is similar to the quotation operator. 

Representing data as code has nice advantages for simplicity, self-validation, optimization, reuse, and laziness or procedural generation. Addending such code is equivalent to editing or transforming the value. Lenses and views are readily applied. 

For connectivity between Awelon project systems, data will uniformly be serialized in this manner. Additionally, a communication context - a value that belongs to the connection - will be maintained at each endpoint, enabling more optimal communication by storing some reusable macros or templates. 

### Capabilities Under-the-Hood

Capability text can be generated lazily, when the capability is serialized to the network. In practice, most capabilities will never be serialized, and thus the capability text never needs be generated. Under the hood, capabilities can be opaque object pointers. 

### Executive Capabilities for Partial Failure

ABC does not have any operators to catch runtime errors. This must is fulfilled by use of 'executive' capabilities, which operate similar to `$` but also admit the possibility of runtime failure - similar to like:

        {exec} :: [x→x']*(x*e) → (x+x')*e

Executives are secured to resist 'defensive programming' strategies in-the-small which should be left to higher architectural layers. Developers can control which subprograms might operate in degraded form. 

### Tail Call Elimination

ABC might be implemented on a stack machine to handle blocks. In that case, [tail call elimination](http://en.wikipedia.org/wiki/Tail_call) is recommended unless stack size is statically bounded. If an applied block ends in `$]` or `$c]`, this is a likely candidate for tail-call elimination. Similarly, ABC has something like an inlining operation: `vr$c`.

### Linear Implementation or Garbage Collection

In ABC, use of drop and copy operations (`%` and `^`) is explicit. We effectively have explicit memory management, but with pure, immutable values. This opens many choices for the implementation:

* linear, deep copy up front, no garbage
* alias on copy, track refcounts, copy on non-linear write
* immutable values in memory, implement ABC as pure functions

Some designs will be more or less efficient than others, or more or less amenable to various optimizations (laziness, interning, memoization, etc.). ABC doesn't specify any particular implementation, and it might be worthwhile for a compiler to experiment empirically and use different techniques in different regions of code. 

### List Terminals as Type Tags

Lists in ABC are generally modeled as ending with a number. However, rather than uniformly ending lists with number 0, I suggest ending lists with a number that hints at their type. Suggested conventions:

* number 0 for generic lists
* number 1 for numerical unit lists
* number 2 for for Huet zippers
* number 3 for text (built into ABC)
* number 4 for sets (order, duplication shouldn't matter)
* number 5 for association lists (list of key→value pairs)
* number 8 for binary (integers in range 0-255)

This convention is easy to track statically and may simplify optimization, visualization, model detection, and type analysis. 

A special case is list-like structures that are intended to have statically known size (tuples, vectors, stacks). These might be terminated using unit, which prevents recursive introspection.

### Sequence Interpretation

In addition to being streamable, a nice feature of a tacit concatenative bytecode is that pattern recognition (at low levels) is often trivial. An ABC stream interpreter might achieve great performance by simply recognizing common sequences of code and substituting a function compiled for that sequence. For example, we might recognize `vrwlc` and execute an optimized full-swap operation, rather than perform each ABC command in sequence. 

More generally, sequence interpretation might be applied to optimize:

* common list operations (map, fold, addend, reverse)
* common association list operations (extract, modify)
* common number operations (exponentiation, matrix multiplication)

The runtime overhead for sequence interpretation isn't very high; it can feasibly be modeled using a finite state machine or trie. An interpreter could have such a machine built-in for known common sequences. An interesting possibility is to dynamically modify this machine based on history for a given instance of the interpreter, leveraging JIT. 

Sequence interpretation is related to ABCD, which will extend ABC with a dictionary of common sequences based on analysis of many projects. However, sequence interpretation is local to the interpreter, and potentially specialized to a problem domain or installation.

## Future Development: Awelon Bytecode Deflated (ABCD)

ABC isn't frozen. As projects, applications, and frameworks are developed, I expect to learn that a few changes might simplify static analysis, improve optimizability, or reduce repetitive code. What is learned will eventually lead to careful evolution of ABC. *The future of ABC is data-driven design.* 

However, ABC should be kept minimal.

I plan to develop a larger bytecode above ABC: ABCD, or ABC Deflated.

ABCD extends ABC with a dictionary that maps UTF-8 characters (in the 2+ octet range) to large, commonly used sequences of ABC. ABCD enables streaming compression of a raw ABC stream against this dictionary. In addition to compression benefits, a carefully designed ABCD dictionary can simplify static analysis and rewrite optimizations by capturing known-safe patterns with high level equational laws. For example, we may introduce operators to map or fold over lists, or manipulate association lists as row-polymorphic records. 

In presence of ABCD, updates to ABC are only necessary for new primitive concepts that could not otherwise be expressed. ABCD addresses bytecode layer performance and parsimony.

Development of ABCD shall be an incremental and empirical effort, based on actual data, and with attention to newly popular data structures and patterns, and constrained by concerns about growing a dictionary too large. There is a point of diminishing returns for larger dictionaries. But I suspect we could do a great deal of good with the UTF-8 two octet range (almost two thousand elements). 

ABCD, external `{#secureHash}` resources, and streaming compression below UTF-8 (e.g. [DEFLATE](http://en.wikipedia.org/wiki/DEFLATE)) seem to fulfill largely orthogonal roles for compression and performance:

* Secure hash resources for large templates, libraries, frameworks
* ABCD covers shorter sequences encountered frequently across projects
* UTF-8 compression covers embedded text, capabilities, local patterns

In addition, compression can be explicitly modeled within an ABCD stream, though it won't be profitable until dictionary lookups are cheap to express in ABCD. Anyhow, between these compression layers, I believe streaming Awelon projects can be very performance competitive for streaming control and data.

## Ambiguous Awelon Bytecode (AMBC)

Ambiguous ABC (AMBC), is an extension to ABC to directly express AO's ambiguity feature. Essentially, ABC is extended with `(|)`, which operate exactly as they do in AO. E.g. `vr(wl|>M)c` has two potential meanings - the left always swaps two values, the right conditionally swaps them. Ambiguity must properly nest with blocks. In some contexts, ambiguity becomes an asset for rapid prototyping, exploratory programming, adaptive code, and incremental refinement. (But caveat emptor: ambiguity hinders performance and reasoning.)

The choice of meanings is not deterministic, but is constrained by typeful context and guided by heuristics. The set of meanings is always finite, but potentially intractable - e.g. it is trivial to construct short AMBC programs that represent spaces of 2^100 ABC programs. There are useful techniques for searching large spaces: hill climbing, repeated local search, genetic programming, etc.. AMBC never guarantees an 'optimal' choice of program, but will often choose well enough.

My intuition is that AMBC is not very suitable for streaming code. The typeful context to disambiguate earlier options are often unavailable until later in the stream. This might be an area worth exploring, but in general AMBC should not be accepted outside of special programming environments.

AMBC can be used with `{#secureHash}` resources, which may themselves be ambiguous (though, naturally, it would be an error to reference ambiguous resources outside of AMBC systems). It may also be used with ABCD dictionaries, though said dictionaries will never be ambiguous. 

