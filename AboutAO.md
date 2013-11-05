# Awelon Object Language (AO)

Awelon Object language (AO) is a tacit concatenative language that operates on a tree-like structure constructed of pairs. AO also enables a variation on logic programming. The language is intended for use in two layers:

1. The first is a thin layer above Awelon Bytecode (ABC) that introduces a concept of words with unambiguous definitions. The semantics of this layer is the static expansion of each word's definition until only ABC remains.

2. The second layer introduces search-based metaprogramming. A word can express a large set of tactics and meanings with different attributes. The semantics of this layer is a non-deterministic static search for a well-typed, high quality expansion.

A word in AO is a unit of modularity and a functional software component. 

AO doesn't use the traditional concept of modules with imports and exports. AO has only words with definitions in a flat, global namespace. The association between a word and its definition is implicit in the programming environment: there is no syntax to define a word locally. The set of words associated with an AO environment is called the dictionary. AO forbids recursive definitions, so the dictionary must be acyclic. 

A definition consists of AO code - a sequence of words, literals, and choice expressions. Juxtaposition is composition. The semantics of a word is simply the inline expansion of its definition.

AO is envisioned for use in a live, wiki-based programming environment. New words are easy to use, document, and define. Automatic visualization should mitigate the normal challenges of tacit concatenative code so users can more easily see the environment. A single dictionary might host hundreds or thousands of projects, with rich cross-project refactoring and words that grow more refined and reusable. Programmers should learn words, not projects! 

## Literals: Numbers, Text, and Blocks

AO supports a few simple number representations. By example:

        42         (integral)
        -12.3      (decimal)
        2.1e-3     (scientific; eN means *10^N)
        34.5%      (percentile; same as e-2)
        1/3        (rational)
        0xca7f00d  (hexadecimal)

These representations are understood as exact rational numbers. 

Additionally, literal numbers in AO are always given a unit structure, represented as data of the form `(number * units)`, where `units` is a static list (potentially empty) of `(label * integer)` pairs. Standard support for units provides better type safety. 

        3`m/s
        1.4e2`kg*m^2
        1/3`apple
        -12.3`C

AO will read simple units as part of a number literal, of the form `x*y/a*b` (which puts b in the denominator), or `1/a`, or `m^N`. Only one `/` character is allowed, and exponents must be between 2 and 9. The semantics for these units, and any unit conversions, is left to user code.

AO supports two formats for text:

        "Block text starts with double quote, by convention
         at a new line. Each continuing line must start with
         a space. On continuing, LF is kept, space is dropped.
         The final line is terminated with `~` at start of the
         line, and LF is dropped. 
        ~
        :inlineLabelText

Essentially, block text in AO is exactly the same as block text in ABC. Inline, label text is much more restricted: it is a pseudo-word whose meaning is just whatever text is after the `:` - in the above case `"inlineLabelText"`. The intention of this secondary format is to simplify expression of labels in data. Empty text may also be expressed this way, using `:` by itself.

Blocks in AO use square brackets, and contain arbitrary AO code:

        [12.3 :foo dup bloop flip trip]

Blocks are free to cross multiple lines, be nested, and so on. Of course, large blocks are a good hint that you should consider refactoring your code. 

AO literals have a slightly different type than ABC literals. 

        In ABC: e -> L * e
        In AO: (s * e) -> ((L * s) * e)

The translation is trivial, but quite beneficial. With this change, type `e` has a stable, relative location and is not "buried" when literals are introduced. This enables words to be developed that assume access to resources in environment `e`. Element `s` can be called "the current stack". 

## Inline ABC

ABC code is inlined using pseudo-words prefixed with `%`:

        %vrwlc      (aka `swap`)
        %lwcwrwc    (aka `rot4`)
        %{&xyzzy}   (annotation)

Inlined ABC in AO may contain most of ABC. The exceptions are as follows: 

* no whitespace (LF, SP)
* no text or blocks
* no numbers (`#0123456789`)
* limitations on capabilities

Except for capabilities, these limitations are no loss of expressiveness. Whitespace in ABC means identity. AO has its own support for text, numbers, and blocks. 

AO requires a dedicated reader mode for capabilities. Upon reading `%{` it must read to the following `}`, including whitespace. This rule ensures AO can syntactically represent the same set of capabilities that ABC can represent. However, capabilities may be rejected by the compiler. At the moment, AO generally permits two classes of capabilities:

* ABC program annotations `%{&xyzzy}`
* AO search attributes `%{:cost:10}`.

Annotations may interact with debuggers, error messages, optimizers, and so on. Search attributes are to help select between ambiguous choices for AO's second layer. Other extensions to AO may later be expressed this way. Tentatively, I might eventually choose to support ABC references, i.e. `%{#secureHash}` (I have mixed feelings about doing so).

## Capability Security

AO prohibits syntactic representation of semantic capabilities, authority bearing or otherwise.

For reasoning about effects, security, portability, configuration, extension, it is preferable that capabilities be distributed through a program as part of the computation. Even for purely functional extensions to AO, such as support for matrices and OpenCL/GPU computations, treating this as capabilities enables substituting an interpreter for porting code to environments that lack these features. 

Fortunately, capabilities can often be distributed through a model by static, compile-time computations. There doesn't need to be a runtime performance penalty for using capabilities. Also, the syntactic effort for threading capabilities is trivial due to AO's nature as a tacit, concatenative language. If capabilities or a powerblock are kept at a stable, relative location, then capabilities have the same syntactic convenience as ambient authority.

AO does have a weakness with respect to Principle of Least Authority: granting authority is not explicit in the syntax. The path of least resistance tends to grant full authority. AO programmers must instead be explicit about where they restrict authority, using blocks and combinators like so:

        [trustMeHehHeh] runInaJailCell

With a little convention, security implications will at least be visible and obvious in code, which is sufficient to follow the principle of least authority whenever it matters. Even better, we a programming environment might help users visualize the environment and how authority is distributed.

## Ambiguity and Sloppy Programming

Words in natural language are often ambiguous. Words may have multiple distinct meanings in different contexts, for example "draw the picture" vs. "draw the curtains" vs. "draw the gun". And words may have very wide meaning that covers many examples, such as "animal" potentially referring to a cat, a dog, a penguin, or a goldfish. Ambiguity is in part resolved by context, and in part left to the imagination of the audience. 

There is a role for sloppy language in programming: rapid prototyping, exploring design spaces, live coding, program or proof search, adaptive code. In many potential use cases, the programmer remains available to clarify problematic ambiguities. Ambiguous language can be terse, and leaving some details to the compiler can alleviate (or delay) burdens on the developer. 

AO's layer two introduces features for expressing and guiding ambiguity. Ambiguity is expressed using `(`, `|`, and `)`. For example:

        a (b | c d) e (f|g)

The meaning of the above expression may be any one of:

        a b e f
        a b e g
        a c d e f
        a c d e g

The choice, made at compile-time, is non-deterministic. However, some expansions can be eliminated if they do not make sense typefully. The compiler's job is to find a sensible expansion, using as much compile-time information as it can.

*NOTE:* Empty expansions are allowed. `(a|)` would effectively express an optional `a`.

*NOTE:* Because AO definitions are acyclic, the set of expansions is finite.

### Guiding Non-Deterministic Choice

Non-deterministic choice is an unsatisfying foundation for sloppy programming. Sloppy programmers aren't seeking just any valid solution. They want the compiler to find a *good* solution, with a balance of useful features, good performance, and tight integration. Further, they desire to explore the solution space and tradeoffs, to observe many sensible programs that fit their sloppy descriptions and choose between them.

To address these concerns, AO provides a flexible mechanism for describing solutions with user-defined attributes, which can later be used for heuristic scoring. These attributes are expressed using the ABC capability model, with a `:` prefix. For example:

        %{:cost:10}
        %{:experimental}
        %{:use-gtk}

Each attribute is assigned a magnitude, a positive integer (default 1). The ability to express relative magnitudes is useful as a tuning feature, but doesn't have any significant semantics. A heuristic function will take these labels and generate scores for different expansions. By tuning the heuristic function, and adding attributes where needed, developers can explore tradeoffs in the solution space.

The mechanism of search is left to the compiler and programming environment. A-star search, forwards and backwards chaining, genetic programming, machine learning of successful search paths, search visualization and human guidance... a great many techniques are feasible. An optimal solution is never guaranteed, and there may still be sone non-determinism. But users should be able to find effective solutions relatively quickly, assuming one exists.

When developers are happy with a solution, the programming environment should make it easy to extract and refactor into an unambiguous vocabulary.

### Controlling Ambiguity

Search can be expensive. Context-dependent and non-deterministic meanings may be semantically troubling (e.g. with respect to equational reasoning). Ambiguity 'features' often aren't. Programmers must easily see and control where these features are used - i.e. keep them in a distinct programming layer. 

To address this, developers may use a corruption model to tag words with ambiguity. For example, a sigil: a word whose definition potentially uses ambiguity has prefix `$`, and a definition using words with prefix `$` potentially uses ambiguity. Such a convention would spread virally through the codebase. It also could be enforced by the AO environment, by issuing a warning or error when it is not followed. Alternatively, we could capitalize ambiguous words, or keep as extra metadata per word. We could use colors or superscripts to visualize ambiguity and other properties. 

I'd like to find what works for users before standardizing, so for now this concern will be left to the programming environment and de-facto standardization.

## Syntax of AO

Parsing code for AO is very simple. AO code is effectively a sequence of words and literals, with a little extra structure for ambiguity. Recognizing numbers is probably the most difficult problem for reading AO. AO currently needs special reader rules for:

* numbers and units
* block text (starting with `"`)
* annotations and capabilities (`%{` to following `}`)
* blocks `[` ... `]`
* ambiguous structure `(`, `|`, `)`

Pseudo-words including label text (`:foo`) and inline ABC (`%vrwlx`) don't need any special reading rules, but must be recognized based on their prefix. 

Words in AO are very flexible in their structure, restricted only to enable easy parsing and printing:

* words cannot start with `:`, `%`, `-`, or a digit
* words cannot contain `"`, `[`, `]`, `(`, `|`, `)`
* words cannot contain whitespace or control characters

Other than that, a programming environment might add a few extra constraints (e.g. so words can be used in URLs), or might unify or normalize some words. But most words should be allowed.

## Standard Environment

AO doesn't enforce any particular environment, except the basic `(s * e)` pair to use literals. However, modifying the environment can require widespread edits to the dictionary. Presented below is an environment that should be effective, efficient, and extensible assuming text-based programming with simple automatic visualization.

        (stack * (hand * (power * ((stackName * namedStacks) * ext))))

* stack - the current stack where operations occur
* hand - a second stack, used as a semantic clipboard
* power - powerblock, source of capabilities and identity
* namedStacks - list of `(name * stack)` pairs
* stackName - name of current stack
* ext - unused, potential for future extensions

By a stack, I mean a structure of the form `(a * (b * (c * ...)))`. List has the same meaning structurally, but lists are typically processed differently than stacks (iteration, fold, search). Names should be static text, usually labels such as `:foo`.

A powerblock is a linear block that contains authority and security policy. It provides features for state resources, to observe or influence the outside world. Access to a powerblock at a stable location provides the syntactic convenience of an ambient authority programming, but also enables precise control, override, and auditing of effects used by distrusted subprograms.

Named stacks can easily be used for most future environment extensions. The `ext` space is reserved for anything that becomes popular enough that a performance boost is desired.

### Data Shuffling

Many common words involve moving values and structures around on the current stack or within the environment. For example, `take` and `put` move objects between stack and hand. `juggleK` and `rollK` rotate objects within the hand or current stack respectively. `:label load` and `:label store` will treat named stacks as a form of global memory. `:label goto` will swap the current stack with the named stack.

Shuffling operations are very first order. However, procedures can be built above them, and they often fade into the background. When coupled with iteration and search, shuffling can offer powerful transformations on the environment.

A single stack is good for a single task. Navigation between stacks is useful when modeling multiple tasks, concurrent workflows that are loosely coupled but must occasionally interact.

### Automatic Visualization

Programmers often reject concatenative languages without even attempting to learn them. My hypothesis is that the learning curve has been too steep: there is a burden to visualize the environment in the head, and a burden to learn a bunch of arcane shuffle words. 

If I am right, automatic visualization and animation of the environment should help with visualization, and the ability to do a little bit of drag and drop on the visualization should help with learning (or ignoring) arcane shuffles. 

AO is intended for use in an environment that provides rich visualization of structure, through the type system and through testing. 

### Documents and Zippers

When developing abstractions, it is best to favor those that are compositional, extensible, scalable, reusable across projects and problems, and for which rich domain-generic vocabularies and analyses can be developed. Documents and document-like abstractions (diagrams, geometries, tables, matrices, grammars, constraint models, rulebooks, lenses, etc.) tend to be much better options than, for example, records and state machines. (If you need a state machine, use a grammar!)

A convention I wish to push for AO is to strongly favor these document-like extensible, compositional, reusable abstractions as the foundation for communicating rich structures between higher level components, even when it means rejecting problem-specific or domain-specific structure that might initially seem more convenient. 

My Hypotheses:

1. reusable, composable abstractions will pay for themselves in the large, even if they initially seem slightly inconvenient in the small

2. manipulating document-like structures on the stacks will work well with standard visualizations, animations, and support effective intuitions and programming-by-example

An interesting feature of document-like structures is that they can be navigated and manipulated incrementally, using a purely functional cursor called a [zipper](http://en.wikibooks.org/wiki/Haskell/Zippers), a data structured developed by Huet in 1997. Thus, navigating and manipulating the environment extends to navigating and manipulating the individual documents. *Effectively, AO's programming environment is analogous to a desktop user environment, with different stacks representing different windows.* 

*TRIVIA:* The navigable environment design for AO was the precursor for developing Awelon environment as a full UI model.

## Documenting AO Code

AO does not have a syntax for comments. The mechanism for AO's documentation is left to convention and the programming environment. I do have a suggested approach: 

Each word `foo` also come with a set of words associated by naming conventions, such as `foo.doc` or `foo.talk` or `foo.test.xyzzy`. These words aren't special from AO's perspective; they simply contain more AO code. However, the behavior of `foo.doc` would involve constructing a document, which could then be rendered and presented to the user. 

Representing documentation as AO code offers many potential advantages:

* trivial parsing, like all AO code
* standared mechanisms for reuse and refactoring
* rich formatting, use of figures and graphs
* flexible templates and frameworks
* potential for interactive documentation

Similarly, tests would have some conventional meanings. 

Documentation directly in code is not recommended for AO systems.

## Automatic Refactoring and Discovery

A significant and often underestimated advantage of tacit concatenative is how easy it is to refactor programs based on simple pattern discovery at the syntactic layer. If five to seven words are observed in a sequence many times, it may be worth refactoring into a new word. Similarly, we can easily seek "alternative" factorings, by searching for patterns in the expanded definitions. 

In addition to finding new factorings, we should expect to find much code that could be factored better based on existing words. These discoveries are useful for didactic purposes, a programmer can begin to learn about existing projects that use similar patterns. 

A good programming environment for AO should be continuously seeking and highlighting opportunities for refactoring, reuse, rewrites. It shouldn't be obtrusive about it, but the opportunities should exist. Similarly, it should support cross-project refactoring and transforms on existing code, based on simple pattern recognition.

## Dissassembly and Translation

An interesting property of AO code is that the dictionary is effectively a grammar. Thus, disassembly of ABC code back into an AO dictionary is quite feasible, based on simple recognition of words or word sequences - effectively, the dictionary parses the program.

This feature could be used both for understanding code and for translating between highly divergent dictionaries.


