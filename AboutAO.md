# Awelon Object Language (AO)

Awelon Object language (AO) is a tacit concatenative language that operates on a tree-like structure constructed of pairs. AO also enables a variation on logic programming. The language is intended for use in two layers:

1. The first is a thin layer above Awelon Bytecode (ABC) that introduces a concept of words with unambiguous definitions. The semantics of this layer is the static expansion of each word's definition until only ABC remains.

2. The second layer introduces ambiguity and search for meaning. A word can express a large set of tactics and meanings with different attributes. The semantics of this layer is a non-deterministic static search for a well-typed, high quality expansion.

A word in AO is a unit of modularity and a functional software component. 

AO doesn't use the traditional concept of modules with imports and exports. AO has only words with definitions in a flat, global namespace. The association between a word and its definition is implicit in the programming environment: there is no syntax to define a word locally. The set of words associated with an AO environment is called the dictionary. AO forbids recursive definitions, so the dictionary must be acyclic. 

A definition consists of AO code - a sequence of words, literals, and choice expressions. Juxtaposition is composition. The semantics of a word is simply the inline expansion of its definition. 

AO is envisioned for use in a live, wiki-based programming environment. New words are easy to use and define. Automatic visualization should mitigate the normal challenges of tacit concatenative code so users can more easily see the environment. A single dictionary might host hundreds or thousands of projects, with rich cross-project refactoring and words that grow more refined and reusable. Programmers should learn words, not projects! 

## Literals: Numbers, Text, and Blocks

AO supports a few simple number representations. By example:

        42         (integral)
        -12.3      (decimal)
        2.1e-3     (scientific; eN means *10^N)
        34.5%      (percentile; same as e-2)
        1/3        (rational)
        0xca7f00d  (hexadecimal)

These representations are understood as exact rational numbers. 

Additionally, literal numbers in AO are always given a unit structure, represented as data of the form `(number * units)`, where `units` is a static list (potentially empty, terminated by numeric 1) of `(label * integer)` pairs. Standard support for units provides better type safety. 

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

Block text in AO is exactly the same as block text in ABC. Inline, label text is much more restricted: it is a pseudo-word whose meaning is just whatever text is after the `:` and before the word break. In the above case `"inlineLabelText"`. The intention of this secondary format is to simplify expression of labels in data. Empty text may also be expressed this way, using `:` by itself.

Blocks in AO use square brackets, and contain arbitrary AO code:

        [12.3 :foo dup bloop flip trip]

Blocks are free to cross multiple lines, be nested, and so on. Of course, large blocks are a good hint that you should consider refactoring your code. 

AO literals have a slightly different type than ABC literals. 

        In ABC: e -> L * e
        In AO: (s * e) -> ((L * s) * e)

The translation is trivial, but quite beneficial. With this change, type `e` has a stable, relative location and is not "buried" when literals are introduced. This enables words to be developed that assume access to resources in environment `e`. Element `s` can be called "the current stack". 

## Inline ABC

ABC code is inlined using pseudo-words, reserving prefix `%`:

        %vrwlc      (aka `swap`)
        %lwcwrwc    (aka `rot4`)
        %{&xyzzy}   (annotation)

Inlined ABC in AO may contain most of ABC. The exceptions are as follows: 

* no whitespace (LF, SP)
* no text or blocks
* no numbers (`#0123456789`)
* no semantic capabilities

Except for capabilities, these limitations are no loss of expressiveness. Whitespace in ABC means identity. AO has its own support for text, numbers, and blocks. 

AO uses a dedicated reader mode for capabilities. Upon reading `%{`, AO should be read to the following `}`, including whitespace. This rule ensures AO can syntactically represent the same set of capabilities that ABC can represent. 

However, most capabilities will be rejected by the AO compiler. 

At the moment, AO permits only annotations and attributes, neither of which have any formal semantics. Annotations can interact with optimizers and debuggers and so on, e.g. `%{&par}` marking a block for parallelism, or `%{&lazy}` for laziness, or `%{&bp}` for a breakpoint. Attributes guide heuristic in case of ambiguity (e.g. `%{:cost:10}`). 

*NOTE:* The word `%` by itself is not allowed. There must be at least one character following `%`. 

## Proper Capability Security

AO prohibits syntactic representation of semantic capabilities. This is a good thing. For reasoning about effects, security, portability, configuration, extension, it is preferable that capabilities be distributed through a program as part of the computation. 

Even for purely functional extensions to AO, perhaps support for matrices, vectors, floating point numbers, and GPU computations, treating this as capabilities enables substituting an interpreter when porting code to environments that lack similar features. 

Fortunately, capabilities can often be distributed through an AO model by static, compile-time partial evaluation. There doesn't need to be a runtime performance penalty for using capabilities. Also, the syntactic effort for threading capabilities is trivial due to AO's nature as a tacit, concatenative language. If capabilities or a powerblock are kept at a stable, relative location, then capabilities have the same syntactic convenience as ambient authority.

AO does have a weakness with respect to Principle of Least Authority: granting authority is not explicit in the syntax. The path of least resistance tends to grant full authority. AO programmers must instead be explicit about where they restrict authority, using blocks and combinators like so:

        [trustMeHehHeh] runJailed

With a little convention, security implications will at least be visible and obvious in code, which is sufficient to follow the principle of least authority whenever it matters. Even better, we a programming environment might help users visualize the environment and how authority is distributed.

## Ambiguity and Sloppy Programming

Words in natural language are often ambiguous. Words may have multiple distinct meanings in different contexts, for example "draw the picture" vs. "draw the curtains" vs. "draw the gun". And words may have very wide meaning that covers many examples, such as "animal" potentially referring to a cat, a dog, a penguin, or a goldfish. Ambiguity is in part resolved by context, and in part left to the imagination of the audience. 

There is a role for sloppy language in programming: rapid prototyping, exploring design spaces, live coding, program or proof search, adaptive or self-optimizing code. In many potential use cases, the programmer remains available to clarify problematic ambiguities. Ambiguous language can be terse. Leaving some details to the compiler can change the burdens on the developer. 

*NOTE:* Edit-time suggestions and type-driven auto-complete can support similar roles. Where sufficient, those techniques should be favored.

AO's layer two introduces features for expressing and guiding ambiguity. Ambiguity is expressed using `(`, `|`, `)`. 

        a (b | c d) e (f|g)

The meaning of the above expression may be any one of:

        a b e f
        a b e g
        a c d e f
        a c d e g

The choice, made at compile-time, is non-deterministic. However, some expansions can be eliminated if they do not make sense typefully. The compiler's job is to find a sensible expansion, using as much compile-time information as it can.

*NOTE:* Empty options are allowed: `(a|)` would effectively express an optional `a`, `(a)` is the same as `a`, and `()` is an identity behavior. More than two choices may be expressed, `(a|b|c)`. Ambiguous choice is fully associative, commutative, and idempotent; organization, ordering, and redundancy for expression of choices has no meaning. 

*NOTE:* Because AO definitions are acyclic, the set of expansions is finite. Of course, the search space is combinatorial and potentially intractable. 

### Guiding Non-Deterministic Choice

Non-deterministic choice is an unsatisfying foundation for sloppy programming.

Sloppy programmers aren't seeking just any valid solution. They want the compiler to find a *good* solution, with a balance of useful features, good performance, and tight integration. Further, they often want to explore the solution space and tradeoffs, to observe many sensible programs that fit their sloppy descriptions and choose between them.

To address these concerns, AO provides a flexible mechanism for describing code with user-defined attributes, which are used for heuristic scoring of search. These attributes are expressed using the ABC capability model, with a `:` prefix. For example:

        %{:cost:10}
        %{:experimental}
        %{:use-gtk}

Each attribute is assigned a magnitude, a positive integer (default 1). The ability to express relative magnitudes is useful as a tuning feature. In addition to user-defined attributes, a compiler can infer some attributes (e.g. program size, estimated efficiency, historical stability). 

A heuristic function will take these attributes to generate scores for different expansions. By tuning the heuristic function, and adding attributes where needed, developers can explore tradeoffs in the solution space. 

The mechanism of search is left to the compiler and programming environment. A-star search, forwards and backwards chaining, genetic programming, machine learning of successful search paths, search visualization and human guidance... a great many techniques are feasible. An optimal solution is never guaranteed, and there may be no obvious winner in the end. But users should be able to find reasonably effective solutions relatively quickly, assuming one exists.

When developers are happy with a solution, the programming environment should make it trivial to extract a specific meaning into an unambiguous word, then refactor it.

### Controlling Ambiguity

Search can be expensive. Context-dependent and non-deterministic meanings can be semantically troubling (e.g. with respect to equational reasoning). Ambiguity features often aren't. 

Programmers must easily recognize and control where ambiguity is used. Search shouldn't be deeply integrated into most AO programs; rather, it should be a distinct programming layer. Ambiguous definitions can use unambiguous words, but not vice versa. 

We can model corruption in the structure of words:

* if a word has a potentially ambiguous definition, it should be capitalized
* if a definition uses a capitalized word, it is potentially ambiguous

The programming environment might emit a warning when a lower-case word is ambiguous. The discipline would spread virally through the AO dictionary. Alternatively, we could use a sigil such as `$`, or keep metadata per word indicating whether ambiguity should raise an error or warning. Colors or superscripts when rendering words could also highlight ambiguity.

I would like to find what works for users in practice, i.e. nice aesthetic, encouraging but not confining. For now, this concern will be left to the programming environment and de-facto standardization. I'll most likely start with capitalization for ambiguity.

## Syntax of AO

Parsing code for AO is very simple. AO code is effectively a sequence of words, literals, and inlined ABC, with a little extra structure for ambiguity. Recognizing numbers is probably the most difficult problem for reading AO. AO currently needs special reader rules for:

* numbers and units
* block text (starting with `"`)
* annotations and capabilities (`%{` to following `}`)
* blocks `[` ... `]`
* ambiguous structure `(`, `|`, `)`

Pseudo-words including label text (`:foo`) and inline ABC (`%vrwlx`) don't need special reading rules, but must be recognized based on their prefix. 

Words in AO are very flexible in their structure, restricted only to enable easy parsing and printing:

* words cannot start with `:`, `%`, `-`, or a digit
* words cannot contain `"`, `[`, `]`, `(`, `|`, `)`
* words cannot contain whitespace or control characters

Other than that, words are generally allowed. A programming environment might encourage a few extra constraints (e.g. so words can be used in URLs), or might unify or normalize some words. But most words should be allowed.

### Structural, Type-Directed Editing

AO's syntax supports flat textual representation, but structured and type-driven editing is both feasible and should be pursued. Programs may be given 'holes' that the editor can help fill with short sequences of words and literals. For many use-cases, edit-time search and auto-completion is a better option than use of ambiguous definitions.

Also, a useful feature would be some zoomability or progressive disclosure. Words that aren't very semantically relevant, such as pure data plumbing, can perhaps be shrunk or faded or replaced with an icon that can be expanded by anyone interested.

### Words for Documentation and Metamodels

AO does not have a syntax for comments.

Documentation for an AO word is expressed by defining another AO word. For example, if we have a word `foo`, we might document it by defining `foo.doc`. The definition of `foo.doc` would construct a document. The programming environment will understand the naming convention, and may present or link the documentation together with the word. 

Modeling documentation in this manner enables reuse, templates and frameworks, rich formatting with figures and graphs, development of interactive or hypertext documentation. It also avoids the challenge of maintaining documentation when refactoring or optimizing code or when using a projectional editor.

A similar philosophy also applies for automatic testing (`test.xyzzy`), asserting equational laws, optimizer suggestions in form of rewrite, IDE plugins or extensions, and live services (`service.foo`). In general, a few naming conventions with AO definitions can go a very long way while staying with a simple dictionary concept.

*NOTE:* Words can also be learned by a good REPL, automatic visualization, tests and examples of use, discovery of words through refactoring. Potentially, we can construct a 'thesaurus' through analysis of structure and sentiment. Documentation should be understood to augment these other approaches, not replace them, and a good programming environment will present these features together with documentation. Many words won't require much documentation. *The primary didactic mechanism in AO should be showing, not telling.*

## Standard Environment

AO doesn't enforce any particular environment, except the basic `(s * e)` pair to use literals. However, modifying the environment can require widespread edits to the dictionary. The environment presented here should be effective and extensible, and well suited to text-based programming environments.

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

AO is intended for use in an environment that provides rich visualization of structure, through the type system and through testing. It should be possible to augment visualization of challenging words by defining extra words (e.g `foo.imagine`) that provide animation rules or code. 

### Documents and Zippers

When developing abstractions, it is best to favor those that are compositional, extensible, scalable, reusable across projects and problems, and for which rich domain-generic vocabularies and analyses can be developed. Documents and document-like abstractions (diagrams, geometries, tables, matrices, grammars, constraint models, rulebooks, lenses, etc.) tend to be much better options than, for example, records and state machines. (If you need a state machine, use a grammar!)

A convention I wish to push for AO is to strongly favor these document-like extensible, compositional, reusable abstractions as the foundation for communicating rich structures between higher level components, even when it means rejecting problem-specific or domain-specific structure that might initially seem more convenient. 

My Hypotheses:

1. reusable, composable abstractions will pay for themselves in the large, even if they initially seem slightly inconvenient in the small

2. manipulating document-like structures on the stacks will work well with standard visualizations, animations, and support effective intuitions and programming-by-example

An interesting feature of document-like structures is that they can be navigated and manipulated incrementally, using a purely functional cursor called a [zipper](http://en.wikibooks.org/wiki/Haskell/Zippers), a data structured developed by Huet in 1997. Thus, navigating and manipulating the environment extends to navigating and manipulating the individual documents. *Effectively, AO's programming environment is analogous to a desktop user environment, with different stacks representing different windows.* 

### Integrating Alternative Environments

The standard environment was developed assuming text-based editing, and relatively simple visualizations. But non-standard environments are certainly feasible, e.g. for frameworks, or for streamable programs as the basis for UI and augmented reality (a goal of the Awelon project). 

The integration point between environments will generally be "software components" that have a relatively narrow interface for inputs and outputs. When the interface is narrow, writing adapter code is trivial.


## Refactoring and Discovery

An often underestimated advantage of tacit concatenative is how *easy* refactoring is at the syntactic or structural layers. When refactoring is easy, it happens often and fluidly, and software can more readily help. Essentially, the [activation energy](http://en.wikipedia.org/wiki/Activation_energy) is lower. 

In AO, users can learn words. A word is often defined by a sequence of five to twelve more words. When a sequence is encountered a few times in a codebase, the programming environment might highlight it for refactoring.

We can also explore spatial aliasing - how we choose to factor the borders between words. Conceptually, we can expand definitions in place and decide whether different borders and boundaries would lead to greater reuse or more comprehensible code. 

A good AO programming environment should greatly aide with refactoring, highlighting opportunities without being obtrusive about it.

And refactoring doesn't need to find new words; if an AO dictionary is used for hundreds or thousands of projects, it is quite possible that another project has already discovered the useful and reusable words that you desire. The greater history a dictionary has, the more such words will be discovered.

Discovery becomes a didactic experience, an opportunity to learn a new word, study how it is used, possibly learn about a project related to your own.

## Dissassembly and Translation

An interesting property of AO code is that disassembly of a large ABC constructs is quite feasible in terms of matching code to a dictionary. Disassembly can be modeled as a parsing or refactoring problem.

This approach could be used both for understanding ABC code, and for translating code between highly divergent dictionaries.


