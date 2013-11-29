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

*Note:* no nesting! These tokens may not contain `{` or `}` characters.

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

A sum type, `(a + b)`, represents that we're either right with `b` or left with `a`. A sum type is constructed by observing a condition. In ABC, primitive operators enable observing a basic conditions, convention punning 'right' with 'true', i.e. `(false + true)` order. These primitives are:

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

Type `0` is identity for sum, called 'void', and corresponds to vacuous condition. Static analysis may infer types for void to reject inconsistencies even in dead code.

We also can distribute, factor, and merge sums:

        D :: a * ((b+c) * e) → ((a*b) + (a*c)) * e -- distrib
        F :: ((a*b) + (c*d)) * e → (a+c) * ((b+d) * e) -- partial factor
        M :: (a+a') * e → a * e -- merge

Full factor is modeled by combining partial factor and merge:

        FM :: ((a*b)+(a'*c))*e → a*((b+c)*e) -- full factor; inverse of D

On merge, the types `a` and `a'` must be compatible for future operations, but they don't need to be exactly the same. What 'compatibility' requires may be judged in context. However, conservatively, any unit values in `a` and `a'` must be structurally aligned. 

Sums may also be copied or dropped (with `^` and `%`) assuming both element types may be copied and dropped.

### Partial Functions and Contracts

ABC provides a simple operator for partiality, assertions, and contracts:

        K :: (a + b) * e → b * e

This operator represents a form of divergence: if we're in the left, that's equivalent to a type error and we'll stop the program as quickly and cleanly as possible. Otherwise we're okay. However, ABC is not restricted to runtime detection of this error. Expressing partial functions with `K` enables ABC to infer dependent types and contracts. If `K` cannot be statically proven safe, the programming environment may issue a warning or raise an error.

### Spatial Temporal Features

(under development!)

Related: 

* [Computing Needs Time](http://www.eecs.berkeley.edu/Pubs/TechRpts/2009/EECS-2009-30.pdf), Edward Lee 2009. 
* [Type Theory for Mobility and Locality](http://www.cs.cmu.edu/~jwmoody/doc/talk/slides-proposal.pdf) by Jonathon Moody.

## ABC Assumptions

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

Fortunately, it is possible to achieve spatial idempotence and causal commutativity even for imperative programming. These properties can be enforced via the effects model, by careful design of capabilities. Affine or linear capabilities - representing threads or objects - renders spatial idempotence irrelevant (since you can't duplicate the expression). Causal commutativity results in minimal, promise-like synchronization with a natural DAG. If necessary, race condition indeterminism can be modeled by [oracle machine](http://en.wikipedia.org/wiki/Oracle_machine) capabilities.

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

Annotations are expressed as capability text, by convention using prefix `&` as in `{&par}`. Annotations must not impact the observable, formal semantics of a program. Annotations may suggest or hint, but not enforce any properties. Despite these limitations, annotations can be useful for many purposes:

* suggest blocks compute lazily
* suggest seq or par for potentially lazy value
* suggest blocks be computed on GPU or FPGA
* suggest use of memoization or caching
* suggest specialization or JIT of a constructed block
* provide hints for proving safety or termination
* assert properties inaccessible to `K`, e.g. purity of a block
* improve blame, error, or warning messages; add a warning
* track locations in original source code
* debugger interaction - breakpoints, visualization, console

When an ABC system doesn't understand an annotation, it should ignore it (treat it as an identity operator) rather than raise an error. If part of an ABC processing pipeline, it should pass the annotation on unchanged since it might be meaningful in a later stage. Some annotations may be lost when aggressively optimizing.

### Capabilities for Structure Sharing and Separate Compilation

It is not uncommon in a project to reuse large, specialized software elements: frameworks, templates, plugins, widgets, diagrams, texts, tables, images, agents, codecs, and other software components. In an ABC stream, the most direct way to reuse code is to repeat it in the stream. Unfortunately, reuse by repetition is inefficient for bandwidth and storage. 

An alternative to repeating code is to name it. Then we can reuse large code by repeating the much shorter name. Unfortunately, most naming systems have properties that repeating code does not: collisions, potential cycles, location dependence, update and version consistency issues. These features are troublesome for security, safety, and distribution. Fortunately, we can address these issues by a more rigorous naming system. Instead of allowing humans pick names, we leverage a [secure hash function](http://en.wikipedia.org/wiki/Cryptographic_hash_function) of the content. Collisions and cycles are effectively eliminated. The 'update' and 'reuse' concerns are cleanly separated. Location is rendered irrelevant.

ABC leverages its effects model to access these `{#secureHash}` sources. 

Here, 'secureHash' will be SHA3-384 of an ABC subprogram, encoded as 64 octets in base64url (`A-Z` `a-z` `0-9` `-_`). When `{#secureHash}` is encountered in the ABC stream, we obtain the associated resource, validate it against the hash, validate it as an independent ABC subprogram (e.g. blocks balanced; text terminates; typesafe), then essentially inline the subprogram in place of the `{#secureHash}` text (albeit, dropping paragraph breaks). The text itself may contain more `{#secureHash}` sources.

To obtain resources, we search local cache or query proxy services. In many contexts, the sender is an implicit proxy; annotations in a stream may suggest extra proxies to search. To improve latency for layers of references, a proxy is free to send a few extra sources that it anticipates will soon be required.

Frequently used sources can be cached in precompiled form for performance. Thus, `{#secureHash}` sources serve as a foundation for separate compilation and linking in ABC. Long term, I envsion that global libraries of highly reusable sources will be developed and refined by automatic factoring, such that there is much benefit of developing in terms of these higher level components.

## Awelon Bytecode Deflated (ABCD)

I plan to develop a larger bytecode above ABC: ABCD, or ABC Deflated.

ABCD extends ABC with a dictionary that maps UTF-8 characters (in the 2+ octet range) to common, widely used sequences. ABC streams can then be compressed against this dictionary. But frequency isn't the only desired characteristic for symbols in this dictionary: a carefully chosen ABCD dictionary will capture known-safe patterns with high level equational laws to simplify static analysis and rewrite optimizations. For example, we may introduce operators to map or fold over lists, or operators for linear algebras and matrix manipulation.

Development of ABCD shall be incremental and empirical, driven by actual data, with attention to newly popular data structures and patterns. Valid concerns include that we should not grow the dictionary too large, and we should not assign operators that might later be deprecated or proven incorrect. UTF-8 can support more than a million elements, but I imagine ABCD will never grow much beyond 1600 functions. Initial ABCD operators will be in the two octet range, which has almost 2000 elements. 

ABCD is intended to be used together with `{#secureHash}` sources. ABCD is suitable for relatively short, frequent, widely used operations. Sources are suitable for large, project-specific components or configurations. Between these features, ABC can be minimal without concern for performance or parsimony.

## Ambiguous Awelon Bytecode (AMBC)

Ambiguous ABC (AMBC), is an extension to ABC to directly express AO's ambiguity feature. 

Essentially, ABC is extended with `(|)` characters, which operate exactly as they do in AO. E.g. `vr(wl|>M)c` has two potential meanings - `vrwlc` (swap) or `vr>Mc` (sort2). In some contexts, ambiguity can become an asset for rapid prototyping, exploratory programming, adaptive code, and incremental refinement. However, ambiguity must be used with caution: it can be difficult to reason about and expensive to resolve. 

In most use-cases, ambiguity should be resolved statically. Relevantly, if a block containing ambiguous code is copied, both copies should use the same meaning.

The resolution of ambiguity is not deterministic, but is constrained by typeful context and guided by heuristics. The space of possible meanings is finite but potentially intractable. There are useful techniques for searching large spaces: hill climbing, repeated local search, genetic programming, etc.. AMBC never guarantees an 'optimal' choice of program, but should choose well enough if the heuristics are good and valid solutions are easy to find. In a live coding scenario, we might heuristically value 'stable' solutions that are similar to prior solutions.

AMBC is unsuitable for streaming, but can be used with ABCD or `{#secureHash}` sources. 

