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

This visibility seems useful for didactic purposes and debugging. For similar reasons, ABC supports whitespace (LF (10) and SP (32)) by assigning it the meaning 'identity', to simplify formatting of ABC. 

### ABC Paragraphs

ABC encourages an informal notion of "paragraphs". A paragraph separates a batch of code, serving as a soft, discretionary indicator of "this is a good point for incremental processing". A well-behaved ABC stream should provide relatively small paragraphs (up to a few kilobytes), and a well-behaved ABC stream processor should process a whole number of paragraphs in each step.

A paragraph is expressed by simply including a full, blank line within ABC code. I.e. LF LF in the stream. Much like a paragraph in a text file.

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

For open or distributed systems, the token should be cryptographically secure and specific to an environment - e.g. encrypted text, signed text, or secure random GUID - such that access to effects is [securable](http://en.wikipedia.org/wiki/Capability-based_security). 

Environment-provided operators may be first-class. For example, `[{obj:SecureRandomGUID}]` might serve as a reference to a specific object in the environment. Applying this block would essentially result in passing a message to the object.

(*Note:* no nesting! tokens may not contain `{` or `}` characters.)

## ABC Behavior Details

### Basic Data Shuffling

ABC provides a minimal set of primitive operators for block-free structure manipulation. The primary structure in ABC is the product (pair), type `(a * b)`.

        l :: (a * (b * c)) → ((a * b) * c)
        r :: ((a * b) * c) → (a * (b * c))
        w :: (a * (b * c)) → (b * (a * c)) 
        z :: ((a * b) * (c * d)) → ((a * c) * (b * d))
        v :: a → (a * 1)
        c :: (a * 1) → a

There are other minimal sets with fewer operators, but this set has some nice symmetry properties. The operators `lzrw` are sufficient for all data shuffling where the rightmost element is sticky, and `v` can displace the rightmost element.

Example encodings:
        
        lzrw :: (a * (b * (c * d))) → (c * (a * (b * d))) -- rot3
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

ABC has two identity operators: SP (32) and LF (10) have type `x → x`. Effectively, whitespace in the ABC stream may be ignored. Additionally, the empty program performs no operations and is thus equivalent to identity.

Tabs or carriage returns are not valid ABC operators. If encountered in an ABC system, an error should be raised as for any other invalid operation.

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

A sum type, `(a + b)`, represents that we're either right with `b` or left with `a`. A sum type is constructed by observing a condition. In ABC, primitive operators enable observing coarse structure and comparing values:

        P :: (Observable x) ⇒ x * e → (x+x(a*b)) * e -- x is pair
        S :: (Observable x) ⇒ x * e → (x+x(a+b)) * e -- x is sum
        B :: (Observable x) ⇒ x * e → (x+x([a→b])) * e -- x is block
        N :: (Observable x) ⇒ x * e → (x+x(N(a))) * e -- x is number
        > :: (Comparable x y) ⇒ x * (y * e) → ((y*x)+(x*y)) * e -- y > x
            #4 #2 > -- observes 4 > 2. Returns (N(2)*N(4)) on right.

By convention, these pun 'right' with 'true'. 

Most types are observable and comparable. The exceptions are:

* blocks are not comparable
* unit is not observable
* unit may be compared only with unit (and is equal)

The limitations on unit are discussed later, regarding *semi-static structure*. 

Anyhow, pairs are greater than numbers, and numbers are greater than sums. If two `(x*y)` pairs are compared, the `x` elements are compared first, and the `y` elements only if `x` is equal. If two `(x+y)` sums are compared, the inner elements are compared only if branches match; otherwise, right is greater than left. The operands to a comparison are returned, sorted in `(min*max)` order.

After a condition is observed, we can conditionally apply a block:

        ? :: (Droppable b) ⇒ b@[x→x'] * ((x + y) * e) → (x' + y) * e

Note that conditional application counts as dropping a block. A relevant block may not be applied in this manner. To apply a block to other conditions requires a set of data shuffling operators for sums:

        L :: (a + (b + c)) * e → ((a + b) + c) * e
        R :: ((a + b) + c) * e → (a + (b + c)) * e
        W :: (a + (b + c)) * e → (b + (a + c)) * e
        Z :: ((a + b) + (c + d)) * e → ((a + c) + (b + d)) * e
        V :: a * e → (a + 0) * e
        C :: (a + 0) * e → a * e

Type `0` is also called void, and corresponds to a vacuous argument. All operations on void return void, though a static analysis should infer a type for the void to detect inconsistencies in dead code.

We also can distribute, factor, and merge sums:

        D :: a * ((b+c) * e) → ((a*b) + (a*c)) * e -- distrib
        F :: ((a*b) + (c*d)) * e → (a+c) * ((b+d) * e) -- partial factor
        M :: (a+a') * e → a * e -- merge

On merge, the types `a` and `a'` must be compatible for future operations, but they don't need to be precisely the same. What compatibility requires may be judged in context. Partial factor and merge must be used together to fully factor:

        FM :: ((a*b)+(a'*c))*e → a*((b+c)*e) -- full factor; inverse of D

Sums may also be copied or dropped (with `^` and `%`) assuming both element types may be copied and dropped.

### Metaprogramming: Static Conditions and Assertions

Partial evaluation is a simple but effective approach to compile-time metaprogramming. Further, it works very well with ABC's assumptions of causal commutativity and fast and loose reasoning. A great deal of a program's behavior can often be computed in advance. Even capabilities might partake if they have some compile-time aspects.

An interesting property of static conditions is that, since a programmer is around to validate a choice, we can relax compatibility requirements for merge. When merging static conditions with `M` we may reduce compatibility concerns to warnings. Or suppress them entirely, depending on context and severity.

We can also *assert* that specific conditions hold:

        K :: (a + b) * e → b * e

Operator `K` essentially says "prove me right". If we're in the left branch, there's an error in the program, similar to a type error. Expressing partial functions with `K` enables ABC to support rich concepts of dependent typing, i.e. a program is proven safe by proving we're always in the right branch at each `K`. If we are in the left branch at runtime (due to incomplete safety analysis in an open system) then we'll halt the program (cleanly, if possible, preferably *before* the error occurs by leveraging a transactional or rollback mechanism). 

As a simple convention, we might also record a message in the environment such that, when an error is detected, a text message is visible in the environment:

        wKw% :: Text * ((a + b) * e) → b * e

Naturally, the message would be eliminated at compile time if we can ensure a safe static condition. And if there is an error or warning at compile-time, the text would be accessible to the developer.

### Spatial-Temporal Features

ABC models spatial properties in terms of logical partitions, and temporal properties in terms of relative latencies. Within a product or sum, the different elements may have different spatial-temporal attributes - for example, we can have types of the form `(Number@CPU * Number@GPU)`. In addition, the product or sum as a whole may have a location, with regards to where it can be introspected.

In general, information about spatial-temporal attributes is *privileged*. There are no ABC operators to query when or where a value is computed.

 capabilities, ABC behavior cannot depend on when or where it executes. There is no way to ask a number when or where it is computed.

modulo special capabilities, ABC code may not introspect when or where a value is computed. 

Additionally, the product or sum itself has a concept of spatial-temporal 'evidence' - i.e. regarding when and where knowledge that the product is a product becomes available. This is important, for example, when loading text on a remote machine.

In general, information 

For the most part, spatial properties are not directly accessible from ABC. 

 However, ABC does require some conventions to assert properties about location, to support 


The spatial properties are not directly accessible from ABC. Communicating between partitions is considered an effect, and is thus controlled by use of capabilities. 


Question: how is ABC involved with spatial properties? Is there some conventional model of capabilities I need for typechecking?

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




## ABC Qualities

This section discusses a few high level properties of ABC's design.

### Semi-Static Structure

Operator `P` in ABC corresponds to `pair?` in Scheme. Like Scheme, ABC can leverage pairs for rich dynamic structure - lists and trees of arbitrary size and structure. Unlike Scheme, ABC can easily control dynamic structure by use of unit type.

The fundamental concept of unit is *structure without information*. I.e. `(a*1)` or `(1*(a*1))` should carry just as much *information* as `a`, but certainly isn't equivalent to `a`. In a system with introspection on structure, this seems to be a problem: the structure itself carries information! We can presumably use `P` to distinguish `1` vs. `(1*1)`.

In ABC, this dilemma is resolved by making unit non-observable. If we apply `P` to `(1*1)`, we get an affirmative. If we apply `P` to `1`, we get a type error. Similarly, unit may only be compared with unit, and is always equal. Our inability to observe unit effectively shifts implicit structural information from the runtime to the compile-time, where it is no problem. 

Leveraging unit, developers can express which data structures are recursively observable, vs. where static knowledge of structure is required. Hypothetically, optimizers can take advantage of structural constraints and common patterns (e.g. static sized vector and matrix operations).

### Monotonic Observations

Many programming languages reduce observations to boolean types, i.e. type `(1 + 1)`, then select conditional behavior based on this structure. Squeezing behavior through the boolean is problematic in many ways: a great deal of information is lost and must be explicitly recovered, and becomes difficult to track after a few more booleans are in scope. Robert Harper has, appropriately, called this phenomenon [boolean blindness](http://existentialtype.wordpress.com/2011/03/15/boolean-blindness/).

In general, ABC avoids this problem. The basic structural observations, `PSBN`, return the element observed - i.e. `P :: x * e → (x+x) * e` - except that we've gained a little information: on the right, `x` is certainly a pair; on the left, `x` is certainly not a pair. Comparison of values also returns the elements observed, as `((y*x)+(x*y))`.

Observations in ABC are not lossy; they strictly increase the amount of information. A consequence, however, is that ABC doesn't much use booleans. I.e. `#4 #3 >` ≠ `#3 #2 >` because, even though they are both 'right', they are right for different reasons. 

If developers desire to treat comparisons as booleans, they must explicitly scrub the information down to `(1 + 1)`.

### Causal Commutativity and Spatial Idempotence

ABC universally assumes causal commutativity and spatial idempotence.

Causal commutativity means that there is no ordering relationship unless there is a visible dependency where the output of one behavior is the input of the next. This property is extremely valuable for optimizations (cf. [Causal Commutative Arrows and their Optimization](http://haskell.cs.yale.edu/?post_type=publication&p=72) by Hai Liu, Eric Chang, Paul Hudak). 

        conventional commutativity: (ABC does not have)
            foo bar = bar foo
        causal commutativity: (ABC assumes / requires)
            [foo] first [bar] second = [bar] second [foo] first
              where first  :: [a→a'] * (a*b) → (a'*b)
                    second :: [b→b'] * (a*(b*c)) → (a*(b'*c))

Spatial idempotence means that, if the same action is performed twice with the same inputs, there is no additional observable impact. This property is also extremely valuable for optimizations, e.g. in content distribution networks. 

        conventional idempotence: (ABC does not have)
            foo foo = foo
        spatial idempotence: (ABC assumes / requires)
            [foo] first dup = dup [foo] first [foo] second
              where first  :: [a→a'] * (a*b) → (a'*b)
                    second :: [b→b'] * (a*(b*c)) → (a*(b'*c))
                    dup    :: (x * e) → (x * (x * e))

ABC is designed primarily for reactive demand programming (RDP), which has both spatial idempotence and causal commutativity universally. ABC optimizers and refactoring tools will assume these properties even when ABC is not targeting an RDP behavior. Fortunately, it is not difficult to leverage linear values to thread effects in imperative code, such that it becomes compatible with spatial idempotence and causal commutativity. Interestingly, this approach is a great deal more precise regarding concurrency and synchronization than traditional models of imperative. 

Spatial idempotence and causal commutativity are valuable because they enable a high level of equational reasoning even in the presence of side-effects. Effectively, developers have most of the reasoning benefits of purity without any of the overheads. 

### Fast and Loose Reasoning for Termination

ABC favors a philosophy of 'fast and loose reasoning' about termination properties. (cf. [Fast and Loose Reasoning is Morally Correct](http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html), Danielsson, Hughes, et al. POPL 2006.) The idea is that - with respect to optimizations, equational laws, rewriting, loop fusion, parallelization, laziness - we should assume every subprogram terminates (or is intended to). This assumption simplifies reasoning. 

To enforce this assumption, ABC compilers should perform termination analysis. 

ABC is Turing complete, so we can't expect a decision to be reached for every program. However, termination analysis is really about avoiding errors in reasoning. And *most errors aren't subtle*. Many silly errors can be detected. A warning, issued when termination could neither be proven nor disproven (within a limited computational effort), can help developers realize where more attention is required.

For ABC, long-running imperative processes shall generally be modeled either as short-running incremental processes (see conventions, below). This is very useful for reasoning about progress, termination, and process control.

Termination is a weak property. In practice, we often wish to reason about performance characteristics: latency, memory requirements, resource usage. Ackermann function terminates, but not within reasonable bounds. However, termination is a good start.

### Implicit Concurrency

Between causal commutativity and fast and loose reasoning, ABC supports a high level of implicit concurrency and non-strict semantics. This is formalized further with spatial-temporal attributes, which can model operations occurring simultaneously at different locations. 

The usual difficulty with implicit concurrency is that we also have implicit synchronization, and if synchronization is fine-grained then we often lose more than we gain by attempting concurrency. Consequently, implicit concurrency is often better leveraged by modeling a concurrent system. By explicitly modeling concurrency - e.g. with workflows, pipelines, incremental processes, RDP behaviors, or forkable thread objects - we can minimize implicit synchronization, and we can provide systematic annotations regarding where parallelism is best introduced.

Thus, in practice, abstractions above ABC systems will generally be explicit about concurrency. But there's a nice qualitative benefit: developers of concurrency abstractions don't need to deal with any painful APIs for synchronization.

### Tacit Concatenative Structure

### Capability-Based Security

### Flexible Type Analysis

ABC has an implicit type system consisting of structural types (pair, sum, unit, void, number, block), substructural types, and a few modal types for spatial-temporal attributes. ABC is subject to termination analysis to validate fast and loose reasoning. Further, ABC's assertion model, operator `K`, enables expression of ad-hoc contracts. 

ABC can be executed as dynamically typed code.

To be streamable, every error must be observable at a specific location in the stream. If we try to observe a unit value, or compare blocks, or apply `K` when we aren't right, then we know we are wrong. If a type failure occurs at runtime, the full ABC program will be halted as quickly and cleanly as possible. (System and architecture layers may use 'executive' capabilities to provide a boundary for partial failure.)

However, ABC is designed for static typing. There are several errors that cannot be avoided by dynamic introspection. In a streaming scenario, we might spend a few cycles analyzing each paragraph for any obvious errors.

ABC does not specify a type inference algorithm.

Programs for validating ABC streams will evolve, often together with optimizers for ABC. Types can be inferred from partial functions and folds. Typechecking can be augmented with pattern recognizers that detect common structures, algorithms, and architectures. I hope, eventually, that analysis systems can reason much like humans do, using multi-level reasoning to argue that systems are correct or incorrect. 

This leads to a philosophy: 

* prove it right, pass it silently
* prove it wrong, raise an error
* indecision, issue a warning
* incrementally improve strategies to decide more programs





 finite set of strategies, or consuming a quota of compute resources. 



ABC systems shouldn't second-guess developers unless they can *prove* them wrong. On the other hand, ABC systems shouldn't lie to developers, either, by pretending everything is okay without *proof*. 

ABC programs aren't wrong unless you can prove them wrong... but they also aren't right unless you can


In addition to low level, precise types, we might recognize known safe patterns of behavior at a much higher level. We can potentially achieve very organic transitions between high level architectures and low level structures, and much deeper optimizations.


The ability to recognize and leverage common structures, folds, algorithms, and design patterns can potentially greatly improve analysis 

This is left to the environment, and it is free to evolve and improve along with any optimizations.

* infer types primarily from partial functions and folds
* recognize or hypothesize common structures, algorithms, patterns
* precise, forgiving; prove safety in context or by partial evaluation
* termination analysis semidecision - pass, warn, error

 And, indeed, multiple algorithms may be applied. Such algorithms are free to develop separately from updates to ABC.

In case of indecision, ABC will generally be forgiving by default. I.e. if we prove there is a problem, we raise an error; if we prove there isn't a problem, we pass silently; if we fail to prove either condition, we may issue a warning. Developers are free to shift warnings to errors through their programming environment or annotations.

The 


 ABC is directed to be relatively forgiving: 

 is designed to support static typing and analysis, but doesn't dictate any particular algorithms. Of course, not just any analysis will do; some moderately advanced features are required to adequately type an ABC system.


ABC

There is no syntax in ABC for declaring types. However, use of assertion operator `K` and observations `PNSB` can help express types or contracts. Similarly, 

, but it is possible to assert conditions on the code. In essense, ABC is designed to support static type safety without ever defining 'type'. 

Pattern recognition features aren't essential, but could result in much greater efficiency for analyzing real bodies of code, and lead to more effective error messages and optimizations for runtime behaviors. For example, we might recognize operations over lists subject to loop fusion, or that a particular association list is never used in an order-dependent manner.


### Type Attributes

The basic types are augmented with a rich set of attributes and dependent types:

* location and latency properties model where and when values can be accessed
* sealed value types model information-hiding and rights amplification
* substructural types for blocks model obligations and resource limitations
* latency constraints for blocks and sealed values - expires, ripens
* track potential ranges for numbers, e.g. protect against divide-by-zero

Attributes can express and enforce some very useful properties. Note: moving values (between locations) and sealing values are both capability based. ABC type systems assume a conventional class of capabilities that enable these features.



### Logically Timed

ABC tracks logical latency properties for atomic values (numbers, blocks). Logical latency is a rational number, indicating a time in seconds. Blocks may also have latency constraints on when they can be invoked. Logical latency is only increased by a logical delay operator. Logical delay simply increments logical latency. 

The relationship between logical latency and real-time is maintained by a scheduler. A good scheduler will keep logical and real time tightly aligned with predictable failure modes, using both soft and hard mechanisms, and some scheduling may occur at compile time. If an effect is invoked on the future, it may be scheduled without invoking it immediately while computation continues elsewhere. Or if a computation is running ahead of where it needs to be, the scheduler may devote more resources to other computations.

The logical model of time, especially on a real timeline (seconds, not arbitrary units), is valuable for understanding and controlling feedback behaviors, for achieving consistent behavior for reactive networks overlays, for comprehending interaction of concurrent effects. However, developers don't always need to think about time. In many cases, the role of assigning temporal properties can be pushed into other layers - networking, effects, frameworks.

Related: [Computing Needs Time](http://www.eecs.berkeley.edu/Pubs/TechRpts/2009/EECS-2009-30.pdf), Edward Lee 2009.

### Data is represented with Code

Structured data in ABC systems is modeled as a stream of ABC code that constructs it. For example, rather than a pair using a structured syntax `(42,108)` we might send code that builds a pair from a unit value: `#108#42lc`. The quotation operator in ABC, `'`, will automatically compute such a constructor for any value.

This approach offers powerful advantages for simplicity, self-validation, compression, and procedural generation. Further, it is much more extensible. To compose code is equivalent to editing the value. The code can be used as history, and formally refactored and optimized for time or space performance. Lenses and views are readily applied. 

For connectivity between Awelon project systems, data will uniformly be serialized in this manner. Additionally, a communication context - a value that belongs to the connection - will be maintained at each endpoint, enabling more optimal communication by storing some reusable macros or templates. 

## ABC Behavior Details

This section has a more detailed description of the ABC behaviors. 

Operators are ascribed with type descriptors in a Haskell-like language. ABC itself has no language for type ascription, but developers can imply structure through partial functions.

ABC's choice of operators is guided by the following desiderata:

* avoid block composition and quotation
* simplify static analysis and type safety
* simplicity, minimality, symmetry, invertibility
* text treated like a first-class type
* similar treatment for products and sums

Performance and parsimony are not strong motivators in ABC's initial design. It's far too difficult to judge such benefits without real metrics for real applications. I also wish to learn how far we can go with typecheckers, optimizers, and compilers that recognize conventional patterns. Parsimony is partially addressed by a conventional use of capabilities to reference ABC resources by secure hash (discussed later). 






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

### Capabilities Under-the-Hood

Capability text can be generated lazily, when it is actually observed via serialization or reflective capability. In practice, many capabilities will never be observed, and thus the capability text never needs be generated. Capability invocations can essentially be replaced by object pointers. 

ABC can potentially distribute many capabilities by partial evaluation at compile-time. The indirection through a block can potentially be removed, the capability code inlined. 

### Tail Call Elimination and Inlining

ABC can be implemented using a stack machine. Each frame on the stack would correspond to a block application, with `$` or `?`, which hides away part or the value and operates on the rest. 

There are many cases where we can easily inline code, e.g. if a block is applied by the pattern `vr$c`. Inlining code can be a powerful technique for performance optimizations. Similarly, there are many cases were we might be able to reduce the stack size, e.g. if a block ends with `$]` or `$c]`, effectively achieving [tail call elimination](http://en.wikipedia.org/wiki/Tail_call).

An ABC implementation is not required to optimize tail calls, but it is encouraged. 

### Linear Implementation or Garbage Collection

In ABC, use of drop and copy operations (`%` and `^`) is explicit. We effectively have explicit memory management, but with pure, immutable values. This opens many choices for the implementation:

* linear, deep copy up front, no garbage
* alias on copy, track refcounts, copy on non-linear write
* immutable values in memory, implement ABC as pure functions

Some designs will be more or less efficient than others, or more or less amenable to various optimizations (laziness, interning, memoization, etc.). ABC doesn't specify any particular implementation, and it might be worthwhile for a compiler to experiment empirically and use different techniques in different regions of code. 

### List Terminals as Type Tags

Lists in ABC are generally modeled as ending with a number. However, rather than uniformly ending lists with number 0, I suggest ending lists with a number that hints at their type. Current conventions:

* number 0 for generic lists
* number 1 for numerical unit lists
* number 2 for for Huet zippers
* number 3 for text (built into ABC)
* number 4 for sets (order, duplication shouldn't matter)
* number 5 for association lists (list of key→value pairs)

Such little conventions are easy to track statically and can simplify optimization, visualization, model verification, and type analysis. 

A special case is list-like structures that are intended to have statically known size (tuples, vectors, stacks). These might be terminated using unit, which prevents recursive introspection.

### Sequence Interpretation

In addition to being streamable, a nice feature of a tacit concatenative bytecode is that pattern recognition is trivial. An ABC stream interpreter might achieve great performance by simply recognizing common sequences of code and substituting a function compiled for that sequence. For example, we might recognize `vrwlc` and execute an optimized full-swap operation, rather than perform each ABC command in sequence. 

More generally, sequence interpretation might be applied to optimize:

* common list operations (map, fold, addend, reverse)
* common association list operations (extract, modify)
* common number operations (exponentiation, matrix multiplication)

The runtime overhead for sequence interpretation isn't very high; it can feasibly be modeled using a finite state machine or trie. An interpreter could have such a machine built-in for known common sequences. An interesting possibility is to dynamically modify this machine based on history for a given instance of the interpreter, leveraging JIT. 

Sequence interpretation is related to ABCD, which will extend ABC with a dictionary of common sequences based on analysis of many projects. However, sequence interpretation is local to the interpreter, and potentially specialized to a problem domain or installation.

## Pitfalls to Avoid

### Conditional Capabilities (Do not!)

When brainstorming, one of my ideas was a convention of the form `{?foo}`, whose output was a sum type depending on whether the environment recognizes `{foo}` as a valid capability. This initially seems neat because it results in code adaptable to the environment. However, the properties are simply *wrong* for adaptive code. 

The question of whether a capability is available should be resolved at the point the capability is acquired, upstream of where it is applied. This enables a bigger picture view of what resources are available for developing adaptive code. This separation of concerns also improves portability, extensibility, configuration management, testing with mockup environments, and security.

### Conventions for Defining Symbols in ABC (Do not!)

Several times, I've been tempted to support a user-defined symbol extension from within ABC. Each time, I determine this is a very bad idea. However, it seems easy to forget why. An example mechanism considered was:

        : :: N(c) * ([x→y] * e) → e  -- DO NOT
          where `c` is a UTF-8 character
          and the block contains meaning of that codepoint

Using a capability to define capability texts is similarly awful. This feature is a bad idea because:

* preventing or detecting cycles is difficult
* scoping properties are unclear, esp. with blocks and composition
* becomes implicit form of abstraction, more than one way to do it
* tracking dictionary context hinders reuse and equational reasoning
* issues regarding symbol maintenance and update
* concerns for security: distrusted software can guess common symbols

I believe these problems arise due to the interaction of definitions with blocks, streaming code, and mutually distrustful software components. However, if we pushed definitions into a lower semantic layer than ABC - e.g. a form of shorthand or compression - I think it will be okay. This possibility is discussed below.

## Future Development: Awelon Bytecode Deflated (ABCD)

ABC isn't frozen. As projects, applications, and frameworks are developed, I expect to learn that a few changes might simplify static analysis, improve optimizability, or eliminate some repetitive code. Such scenarios will eventually lead to careful evolution of ABC. *The future of ABC is data-driven design.* 

To help address performance, analysis, and code repetition concerns - assuming code that can be expressed in ABC normally - I'm contemplating a language ABCD (Awelon Bytecode Deflated) that extends ABC with a dictionary mapping unused characters to common sequences of ABC.

I will take a very large corpus of ABC paragraphs, covering thousands of projects, to systematically discover common subprogram sequences that would be excellent targets for compression. This is more challenging than traditional compression: the possibility exists to rearrange code a little to factor out common structure.

Of the strongest candidates for compression, I would seek sequences with relatively simple types that can be comprehended and documented by humans, and that seem promising to simplify static analysis or performance if treated as primitives. For example, we might optimize encodings for maps and folds over a list, addending lists, testing whether a value is a natural number or positive integer, extracting a record from an association list, addition and multiplication of matrices. 

If this works as well as I hope, then ABCD will become a very effective language for streaming, interpreters, and compilers, while retaining ABC's simple semantics, security, and tacit concatenative structure. ABC would remain the small, trusted kernel targeted for constructive proofs. ABC can be translated to ABCD easily enough by simple parsing or rewrite techniques, and the converse is even easier.

The extension of ABCD would be an incremental effort, with attention to newly popular data structures and design patterns. This will be a slow process, guided by consideration for overheads of language updates and larger dictionaries. Those overheads include costs for upgrading existing systems, plus the costs associated with keeping a larger dictionary for operations that are progressively more rare and specialized. I.e. UTF-8 supports over a million characters, but I doubt we'll ever want million element lookup tables. However, in two octets, UTF-8 supports two thousand characters. I expect we could do a great deal of good for a wide variety of problem domains with a library of two thousand common functions!

I currently favoring the following constraints for future development:

* single octet codes reserved for very rare core ABC extensions
* higher codes will be utilized by ABCD, initially within two octets
* esoteric C0 and C1 to be avoided, or leveraged for out-of-band comms
* ABCD supports extensions for inference, performance, compression 
* ABC only extended for new core features that can't otherwise be modeled

ABCD can be leveraged together with external `{#secureHash}` lookups plus streaming compression algorithms below UTF-8. Secure hash lookups are great for project specific libraries or templates, and they support cached compilation. Streaming compression is excellent great for communicating large amounts of text for human perusal or embedded DSLs, or for compressing capabilities and annotations that appear repeatedly. 

Between these techniques and sequence interpretation, I believe ABC can be very performance competitive for streaming data and control, while retaining nice syntactic and semantic properties.

## Ambiguous Awelon Bytecode (AMBC)

Ambiguous ABC (AMBC), is an extension to ABC to directly express AO's ambiguity feature. Essentially, ABC is extended with `(|)`, which are used exactly as they are in AO. E.g. `vr(wl|>M)c` has two potential meanings - the left always swaps two values, the right conditionally swaps them. In AO, this ambiguity becomes an asset for rapid prototyping, exploratory programming, adaptive code, and incremental refinement. 

The choice of meanings is not deterministic, but is constrained by typeful context and guided by heuristics. The set of meanings is always finite, but potentially intractable - e.g. it is trivial to construct short AMBC programs that represent spaces of 2^100 ABC programs. There are useful techniques for searching large spaces: hill climbing, repeated local search, genetic programming, etc..

My intuition is that AMBC is not very suitable for streaming code. The typeful context to disambiguate earlier options are often unavailable until later in the stream. This might be an area worth exploring, but in general AMBC should not be accepted outside of special programming environments.

AMBC may be used with the ABCD dictionary or `{#secureHash}` resources. 

