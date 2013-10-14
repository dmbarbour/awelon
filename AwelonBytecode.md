# Awelon Bytecode (ABC)

ABC is a tacit, concatenative bytecode represented in a UTF-8 stream. ABC can be typechecked, compiled, and streamed. ABC is designed to be visible, not legible: non-printing characters are not assigned relevant meaning, and a human can see the text and numbers being communicated.

## Format

ABC consists of a stream of primitives and literals. ABC has only three kinds of literals: blocks `[code]`, text `{Hello, World!}`, and numbers.

        [slsls] :: e -> (block*e) 
        {Hello} :: e -> (text*e)  

The number model is actually a pseudo-literal, such that `#42` is three bytecodes. Each digit is a bytecode that multiplies a number on the stack by ten, then adds the digit's value. The code `#` adds a new zero-value to the stack. The result is that `#42` has the number 42 on the stack. 

        # :: e -> (num*e)
        0,1,2,3,4,5,7,8,9 :: (num*e) -> (num*e)

(Alternatively, I could use a text -> num conversion. `{42}#`. This has an advantage of using fewer bytecodes, but a disadvantage of syntactically enabling type errors and perhaps requiring a stronger conversion function than I desire. Also, they're bytecodes I'm hesitant to use anyway.)

A processor for ABC is a simple machine:

1. normal mode, primitive functions represented as bytecodes.
2. block mode, `[` to balancing `]`, containing ABC.
3. text mode, `{` to balancing `}`, containing UTF-8.

Note that text does not support escapes. Text may contain `{` and `}` but only in a balanced manner. Imbalanced text must be modeled by creating balanced text then postprocessing it (to add or subtract characters). This can be performed in a streaming manner.

## Value Types

ABC's core set of value types is very minimal.

* text, a finite UTF-8 sequence `{Hello, World!}`
* numbers, precise rationals `{42}#{12.3}#{1/3}#`
* blocks, containing ABC code `[slsls]`
* products, an indexed pair of values
* unit, identity for product `1`
* sums, an indexed union of values
* void, identity for sum `0`

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

By "stream processing" I mean an ability to execute a program incrementally and sequentially, without ever having the full program in memory. Stream processing is easy for concatenative programming models, but several relevant concerns remain such as:

* type safety, partial failure
* consistent intermediate states
* efficiency and compilation

To address these concerns, ABC provides a simple solution: the `;` operator.

The idea: updates apply in batches. A batch moves a value from one meaningful state to another. But there are intermediate states that should not be observed. A batch should fully succeed or fail - no partial success; atomic batching makes it much easier to reason about system behavior. The `;` operator indicates a valid boundary for one batch. 

The stream provider should inject `;` at appropriate locations. The stream consumer should buffer up to a `;` boundary before processing, or might process more than one batch at a time (if more than one has become available). If the batch is too large, hangs too long, or uses too many resources it can fail as a denial of service issue. 

Use of `;` is weak, informal, discretionary, but effective. Its formal meaning is identity, and its removal only impacts performance characteristics. If developers need strong, semantic guarantees about behavior, they must model it using capabilities. But even then use of `;` makes a good initial step.

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



