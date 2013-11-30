# Awelon Object Language (AO)

Awelon Object language (AO) is a programming language built above Awelon Bytecode (ABC). AO is a concatenative programming language, but is distinguished from other concatenative languages in a many ways: 

* AO is *capability based*. Programs may invoke effects, but access to effects must be granted through a first-class function called a capability. If a subprogram is not granted any capabilities, it must be pure. AO can simulate ambient authority by keeping power at a standard location in the environment.

* AO operates on *pairs* rather than a stack. AO uses pairs to model an extensible multi-stack environment, association lists, and other ad-hoc data structures. Multiple stacks are valuable for modeling multiple tasks and as a staging area for data plumbing. Manipulating the environment enables flexible metaphors and conventions for programming.

* AO assumes a global dictionary. AO has no import/export mechanism, nor any syntax to define a word. The definition of a word must be edited through a programming environment. The meaning of any word is the inline expansion of its definition. The dictionary must be acyclic.

* AO enables inference of rich types. Six structural types - pairs, sums, unit, void, numbers, functions - are the core of AO. Unit type controls against introspection, such that not every structure is searchable. There are also substructural types - functions may be affine or relevant or have expiration properties, and values in general may have spatial-temporal attributes. Advanced analyses may infer dependent types, based on assertions. Numbers are generally tagged with units.

* AO requires *causal commutativity* for effects, which means there is no ordering relationship unless the output of one effect becomes input to another. (`[foo] first [bar] second` = `[bar] second [foo] first`). This property can be enforced by the capability model. It supports a high level of implicit concurrency and parallelism.

* AO requires *spatial idempotence* for effects, which means that duplicating inputs to an effect (at the same logical time) has the same results and no additional impact. (`dup [foo] first [foo] second` = `[foo] first dup`). This property can be enforced by the capability model. Between spatial idempotence and causal commutativity, AO enables effective equational reasoning typically associated with pure functional code.

* AO assumes and requires every subprogram terminates. Non-terminating applications are modeled in terms of incremental processes with an implicit toplevel loops, or as an RDP behavior. Termination isn't decidable in general, but a termination analysis may raise an error if non-termination is proven or issue a warning if termination is not proven. The *fast and loose reasoning* about termination properties simplifies optimization and partial evaluation.

* AO supports declarative meta-programming. An AO subprogram may denote a *set* of possible expansions with different types and attributes. A subset of these expansions should be type safe and meaningful in context. From that subset, one expansion is selected at compile-time that is high quality according to developer-provided heuristics and search. The programming environment must ensure programmers are aware of ambiguity and able to control its use.

A word in AO serves both as a unit of modularity and a functional software component. A programming environment may designate some words as *active*, e.g. based on naming conventions, representing extensions, live services, documentation, automatic tests. 

Each word has a definition. A definition consists of AO code - a sequence of words, literals, inline ABC, and ambiguous choice expressions. Juxtaposition is composition. The semantics of a word is simply the inline expansion of its definition. Ultimately, an AO program expands into Ambiguous ABC (AMBC) plus a few annotations to track location in source code. The AMBC is typechecked, searched, optimized, and typically compiled further for a target platform.

AO is envisioned for use in a live, wiki-based programming environment. New words are easy to define and use, and the environment should help discover useful factorings. Automatic visualization mitigates the normal challenges of tacit concatenative code so users can more easily see the environment. A single dictionary might host thousands of projects, with rich cross-project refactoring and a dictionary of words growing ever more refined and reusable. Programmers should learn words, not projects!

## Literals: Numbers, Text, Blocks

AO supports a range of useful number representations. By example:

        42         (integral)
        -12.3      (decimal)
        2.1e-3     (scientific; eN means *10^N)
        34.5%      (percentile; same as e-2)
        1/3        (rational)
        0xca7f00d  (hexadecimal)

In all cases, these are understood as exact rational numbers. Additionally, AO tags numbers with units, represented as a sorted list of `(dtext * number)` pairs, terminated by number 1.

        1.4e2`kg*m/s^2
        1/3`apple
        -12.3`C

Unit checking for numbers provides an effective alternative to typechecking in many cases. Beyond providing a little structure, AO leaves interpretation or normalization of units to user code. The unit expression is assumed to be of the form `x*y/a*b`, allowing for `1/a`, or `m^N`. One `/` character is allowed, placing everything to its right in the denominator. 

AO supports two formats for text, both starting with `"`.

        "Block text starts with double quote at a new line. 
         Each continuing line must start with a space. On 
         continuing, LF is kept, space is dropped. The final
         line is terminated with `~` at start, LF dropped. 
        ~
            "inline text"

Block text in AO is similar to block text in ABC. Inline text is less flexible: it may not contain `"` or LF characters. Block text must start a new line, and inline text must not. AO has no escapes in text; however, AO developers are free to leverage partial evaluation to post-process the text at compile time. 

There is no concept of escaping text built into AO, though developers are certainly free to create text then statically post-process it. There are many use cases for inline text: labels, short captions, micro DSLs (e.g. for regular expressions), and so on. Inline text may not start immediately after a newline.

Blocks in AO use square brackets and contain arbitrary AO code:

        [12.3 :foo dup bloop flip trip]

Blocks may freely cross multiple lines, be nested, and so on. Though, a large block should be considered for refactoring. The square brackets count as word separators.

AO literals have a slightly different type than ABC literals. 

        In ABC: e -> L * e
        In AO: (s * e) -> ((L * s) * e)

The translation from AO to ABC is trivial, but offers a significant benefit. With this change, type `e` has a stable, relative location and is not "buried" when literals are introduced. This enables words to be developed that assume access to resources in environment `e`. Element `s` might be understood as "the current stack". 

## Inline ABC

ABC code is inlined using pseudo-words, reserving prefix `%`:

        %vrwlc      (aka `swap`)
        %lwcwrwc    (aka `rot4`)
        %{&xyzzy}   (annotation)

The canonical expansion of inlined ABC is simply each ABC operator alone. For example, `%vrwlc` expands to `%v %r %w %l %c`. Capabilities, read from `%{` to the following `}` (including whitespace) are a special syntax case and must always be expressed in their canonical form. AO's inlined ABC in AO may contain most of ABC. The exceptions are as follows: 

* no whitespace (LF, SP)
* no text or blocks
* no numbers (`#0123456789`)
* no semantic capabilities

Whitespace in ABC means identity. AO has its own support for text, numbers, and blocks. AO uses a dedicated reader state for capabilities. So the first three points don't hinder AO. The limitation on capabilities is discussed below.

*Note:* AO does not allow inlining of ABCD. ABCD essentially extends ABC with a fixed dictionary, which is redundant with AO's own dictionary feature. ABC will instead compiled to ABCD as a postprocess, independently of how AO modularizes code.

## Proper Capability Security

AO prohibits syntactic representation of semantic capabilities, i.e. you cannot hard-wire authorities (e.g. to read or write files), nor even pure extensions (e.g. for floating point matrix manipulations), into AO code. This is a good thing! By distributing authorities and extensions through code as a formal part of the computation, AO is far easier to port, configure, maintain (with tests and mockups), and secure.

AO does allow annotations and attributes to be expressed using capability text. Annotations express hints for a compiler, optimizer, debugger, e.g. to support parallelism, laziness, breakpoints, deprecation, typechecking. For example, `%{&par}` might indicate a block should be evaluated in parallel. It is recommended that annotations be used indirectly, e.g. define `par` to mean `%{&par}`.) Attributes are annotations to support heuristic search when an AO program has more than one meaningful (type safe) expansion.

Between compile-time partial evaluation and the ability to simulate ambient authority, AO's capability-based effects model does not hinder performance or inconvenience the programmer.

However, there is a weakness: in AO, the path of least syntactic resistance tends to grant full authority. AO programmers must instead be explicit about where they restrict authority, using blocks and combinators like so:

        [trustMeHehHeh] runJailed

With a little convention, security implications should at least be visible and obvious in code, which is sufficient to achieve the principle of least authority when it most matters. 

## Ambiguity and Search

Ambiguous choice in AO is syntactically expressed by wrapping an ambiguous section in parentheses, and separating one or more options with a vertical bar. For example:

        a (b | c d) e (f|g|h)

The meaning of the above subprogram may be any one of:

        a b e f
        a b e g
        a b e h
        a c d e f
        a c d e g
        a c d e h

Many expansions can be eliminated if they are not *meaningful*, that is if they are not type safe (including context). Of the remaining, valid expansions, one will be chosen heuristically. That is, rather than making a random choice, we search for a valid program that has nice characteristics and qualities according to a developer or configuration. 

To support heuristics, programmers can annotate their code with *attributes*:

        %{&attrib} :: (Attribute x) => (x * e) -> (x * e)

Attributes are statically computable, introspectable values, passed to the `%{&attrib}` annotation - usually a `(label*number)` pair. An invalid attribute will result in a minor warning and be ignored. In addition to user-defined attributes, an AO programming environment may compute attributes regarding stability, size, expected performance. The whole list of attributes is generally passed to user-provided heuristic functions, and subject to a variety of search techniques.

Roles for ambiguous code and program search: rapid prototyping, live coding, exploring design spaces, adaptive code, optimization, tactical theorem proving. Of course, search isn't the only approach: edit-time suggestions and auto-complete can support similar roles. Due to the overhead of search, edit-time techniques should be favored where feasible.

An interesting application of AO's ambiguity is genetic programming. A common class of ambiguous programs has structure amenable to treating choices as genes - i.e. most of the choices are shallow and near the toplevel. We can create populations of viable solutions modeled by vectors, test them, and search for stable, high quality solutions within the specified space of programs.

*NOTES:* `()` is identity, `(a)` is just `a`, and `(a|)` is an optional `a`. Ambiguous choice is fully associative, commutative, and idempotent. The order that choices are expressed has no impact on heuristics. 

### Constraining Ambiguity

Search is expensive. Also, context-dependent meaning can be semantically troubling, e.g. it hinders equational reasoning. Ambiguity is a feature that must be used carefully, and removed from the codebase when it is no longer necessary.

One job of the programming environment is to help developers easily recognize and control where ambiguity is used. Towards this end, I suggest two techniques:

1. ambiguous words are colored or styled differently when rendered
2. automatic tests may introspect dictionary and fail if a word is ambiguous

When a word is ambiguous generally, but unambiguous in context, it might be rendered differently than if ambiguous in context. Overall, this design is simple, flexible, and easily extended for more attributes and properties. 

## Syntax of AO

Parsing AO code is simple. AO code is effectively a sequence of words, literals, and inlined ABC, and ambiguous choices. The most difficult part is parsing numbers. AO currently needs special reader states for:

* numbers and units
* inline or block text
* capabilities, annotations (`%{` to following `}`)
* blocks `[` ... `]`
* ambiguous structure `(`, `|`, `)`

Words in AO are very flexible in their structure. However, words are limited to simplify parsing and printing. 

* words cannot start with `%`, `-`, or a digit
* words cannot contain `"`, `[`, `]`, `(`, `|`, `)`
* words cannot contain C0 or C1 control characters, SP, or DEL.

A specific programming environment might have a few extra constraints, e.g. so words can be used in URLs. We may also unify or normalize some words, or may add a new class of pseudo-words. But most words should be allowed, including UTF-8.

### Flat Namespace

AO has no syntax for imports and exports, nor even a syntax to define words. Instead, AO developers maintain a dictionary through their programming environment. A word, in this dictionary, is both the unit of modularity and a functional software component. (Some words, by naming convention, may also represent environment extensions, automated tests, or live services.) All words in the dictionary in the dictionary are uniformly available: no hierarchy, no context-dependent bindings. 

AO's design has many advantages, including:

* no boiler-plate import/export management
* common language and refactoring across projects
* unifies module, function, and software component
* easy fit for a wiki-based programming environment
* more opportunity for discovery, reuse, knowledge sharing
* conflicts resolved, not avoided: dense namespace, terse code

However, a flat namespace also has a weakness. In some use-cases, we want words to have unambiguous *operational definitions* in a particular context, such as a DSLs or a specific project. Historically, developers in languages with flat namespaces address this concern by use of naming convention, typically a prefix identifying a library. But this solution comes with a terrible price: increased verbosity, decreased readability.

Fortunately, we can address this weakness in a modern programming environment. 

AO has almost no syntax, and minimal need for syntax highlighting. AO programming environments are thus free to use color and style to improve readability and developer awareness in more ad-hoc ways, such as identifying ambiguity properties and types... or as an alternative to rendering full words.

We can configure our programming environments to recognize common prefixes or suffixes, and instead render distinct colors, styles, or icons. This way, we can disambiguate the origin of a word at a glance, without reading disambiguation text. It also becomes easy to see more of the program at once, and to recognize patterns in how words from different DSLs and projects are mixed. To help write these longer words, we can leverage auto-complete. Color also helps for auto-complete, to more quickly identify the desired word. 

AO should have a single dictionary for many thousands of projects. 

Cross-project refactoring then becomes an effective basis for discovering or creating common words that need no prefix, or perhaps be part of a common DSL. Developers are free to use project specific words to start, then slowly shift reusable content into shared spaces.

### Documentation and Learning AO

AO does not have a syntax for comments.

Documentation for an AO word is expressed by defining another AO word. For example, if we have a word `foo`, we might document it by defining `doc.foo`. The programming environment will understand the naming convention, and may present or link the documentation together with the word. 

The documentation is not plain text. It is AO code, a small program that constructs a document. Modeling documentation in this manner simplifies reuse, templates and frameworks, rich formatting with figures and graphs, development of interactive or hypertext documentation. It also avoids the challenge of maintaining documentation when refactoring or optimizing code or when using a projectional editor.

*NOTE:* Words can also be learned by a good REPL, automatic visualization, tests and examples of use, discovery of words through refactoring. Potentially, we can construct a 'thesaurus' through analysis of structure and results. Documentation should be understood to augment these other approaches, not replace them, and a good programming environment will present these features together with documentation. Ideally, most words won't require much documentation. *The primary didactic mechanism in AO should be showing, not telling.*

### Testing and Environment Extensions

Similar to documentation, AO systems leverage naming conventions for testing (`test.xyzzy`), asserting equational laws, to suggest optimizations or rewrite rules, IDE plugins or extensions, even live services (`service.foo`). 

A good programming environment should make it easy for users to express and validate high level (i.e. quantified - forall, exists) properties about their dictionaries. For AO, this expression will occur via the testing environment. Tests in AO aren't limited to operating within the language; tests can leverage a suite of capabilities for introspection or reflection. (Testing capabilities are generally inaccessible outside the testing environment.) Leveraging introspection, tests can potentially verify high level properties both through symbolic analysis and QuickCheck-like mechanisms.

By reflecting on the dictionary and test system itself, it is feasible to ask whether a word phrase is ambiguous, or whether certain other tests have passed.

The use of naming conventions as a basis for extending an AO environment and editor is a very simple, very powerful approach. It keeps the extension code readily accessible within the language, subject to the same testing and maintenance mechanisms as everything else.

### Structural, Type-Directed Editing

AO's syntax supports flat textual representation, but structured and type-driven editing is both feasible and should be pursued. Programs may be given 'holes' that the editor can help fill with short sequences of words and literals. For many use-cases, edit-time search and auto-completion is a better option than use of ambiguous definitions.

Also, a useful feature would be some zoomability or progressive disclosure. Words that aren't very semantically relevant, such as pure data plumbing, can perhaps be shrunk or faded or replaced with an icon that can be expanded by anyone interested.

## Standard Environment

AO doesn't enforce any particular environment, except the basic `(s * e)` pair to use literals. However, significant structural modifications of an environment require widespread edits to the dictionary. To help avoid this, I've developed (after several iterations) an environment that seems effective, extensible, and usable for text based programming:

        (stack * (hand * (power * ((stackName * namedStacks) * ext))))

* stack - the current stack where operations occur
* hand - a second stack, used as a semantic clipboard
* power - powerblock; source of authority, state, identity
* namedStacks - list of `(name * stack)` pairs
* stackName - name of current stack
* ext - unused, potential for future extensions

By a stack, I mean a structure of the form `(a * (b * (... * 1)))`. List has similar structure, but terminate with a number instead of unit (and thus may be recursively manipulated). Names should be static text, easy to remember, e.g. labels such as `"foo"`.

The powerblock is the generic entry point to observe or influence the real world. Utilizing power is (generally) a multi-step process that involves acquiring a specific capability then applying it. For abstraction and security purposes, functions are often granted specific capabilities on the stack. A powerblock also can be forked so a distrusted subprogram receives a child powerblock, which may be constrained in general terms before forking. Access to a powerblock at a stable location provides syntactic convenience similar to ambient authority.

Named stacks can easily be used for most future environment extensions. The `ext` space is reserved for structures that becomes popular enough that a fixed relative location is desired.

### High Level Data Shuffling

Many common words involve moving values and structures around on the current stack or within the environment. For example, `take` and `put` move objects between stack and hand. `juggleK` and `rollK` rotate objects within the hand or current stack respectively. `:label load` and `:label store` will treat named stacks as a form of global memory. `:label goto` will swap the current stack with the named stack.

Similarly, I expect AO will heavily leverage Huet zipper-based abstractions, which are essentially a form of data shuffling.

Shuffling operations are very first order. However, procedures can be built above them, and they often fade into the background. When coupled with iteration and search, shuffling can offer powerful transformations on the environment.

A single stack is good for a single task. Navigation between stacks is useful when modeling multiple tasks, concurrent workflows that are loosely coupled but must occasionally interact.

### Automatic Visualization

Programmers often reject concatenative languages without even attempting to learn them. My hypothesis is that the learning curve has been too steep: there is a burden to visualize the environment in the head, and a burden to learn a bunch of arcane shuffle words. 

If so, automatic visualization of environment structure, and animation of how it changes across AO code, should relieve much of this burden. 

Conversely, ability to use a little drag and drop or copy-paste through the environment visualization could help specify data shuffling in the tricky cases and when first getting used to the concept. It would be a weak form of programming by example.

### Document-like Structures, Zippers and Folds

When developing abstractions, it is best to favor those that are compositional, extensible, scalable, reusable across projects and problems, and for which rich domain-generic vocabularies and analyses can be developed. Document-like abstractions - diagrams, geometries, tables, matrices, grammars, constraint models, rulebooks, lenses, scene-graphs, etc. - tend to be much more composable, extensible, and scalable than, for example, records and state machines. (In some cases, they are also more general. E.g. grammars can model state machines, and tables can model records.)

A convention I wish to push for AO is to strongly favor these document-like extensible, compositional, reusable abstractions as the foundation for communicating rich structures between higher level components, even when it means rejecting problem-specific or domain-specific structure that might initially seem more convenient. 

Hypotheses:

1. reusable, composable abstractions will pay for themselves in the large, even if they initially seem slightly inconvenient in the small

2. manipulating document-like structures on the stacks will work well with standard visualizations, animations, and support effective intuitions and programming-by-example

An interesting feature of document-like structures is that they can be navigated and manipulated incrementally, using a purely functional cursor called a [zipper](http://en.wikibooks.org/wiki/Haskell/Zippers), a data structured developed by Huet in 1997. Thus, navigating and manipulating the environment extends to navigating and manipulating the individual documents. *Effectively, AO's programming environment is analogous to a desktop user environment, with different stacks representing different windows.* 

An interesting convention to help specify datatypes is to model folds: 


## Refactoring

There is significant utility of being able to refactor at the syntactic layer, i.e. by copy-and-paste or search-and-replace of common code segments. When refactoring is easy, it happens often and fluidly and is more readily aided by software. AO simplifies this further by favoring a flat namespace - there is no need to parse context to comprehend bindings.

An AO programming environment should help developers refactor, e.g. highlight sequences of five to twelve words that are seen many times in a codebase, or sequences that already have dedicated names. In the latter case, the AO programming environment would implicitly help developers discover and learn existing words. 

Beyond simple pattern matching, we must also concern ourselves with the boundaries, how definitions are aligned and coupled with words. For example:

        word1  word2            word3  word4  e
        a b c  d e      (vs)    a b    c d 

Well chosen boundaries results in more comprehensible, reusable code. AO words are at once functions, modules, and software components. Thus the boundaries correspond to interface and coupling decisions. Unfortunately, whether boundaries are well chosen is often unclear before there is a great deal of reuse in a broad variety of contexts. 

An AO programming environment should also support developers in exploring different arrangements of boundaries, in addition to other equivalent expressions of the same behavior based on blocks or causal commutativity.

## Dissassembly and Translation

An interesting property of AO code is that disassembly of a large ABC constructs is quite feasible in terms of matching code to a dictionary. Disassembly can be modeled as a parsing or refactoring problem. This feature could be used both for understanding ABC code, and for translating project code between highly divergent dictionaries.

