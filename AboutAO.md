# Awelon Object Language (AO)

Awelon Object language (AO) is a programming language built above Awelon Bytecode (ABC). AO is a concatenative programming language, but is distinguished from other such languages in several ways: 

* AO is not stack-based. It can operate on any value structured by pairs.
* AO leverages pairs to model multiple stacks, Huet zippers, lenses, tools.
* AO is capability based. There is no ambient authority for side-effects.
* AO is gradually typed. Supports static analysis and type inference.
* AO is structurally and substructurally typed; type model is compositional.
* AO exhibits *causal commutativity*, which enables implicit parallelism.
* AO exhibits *spatial idempotence*, which simplifies equational reasoning.
* AO can represent adaptive or declarative search-spaces of programs. 

In AO, a **word** is both a unit of modularity and a functional software component. A word has a definition. The basic semantic for any word is to expand into its definition. Valid definitions are acyclic. At the limit, code expands into literals and inlined ABC. The relationship between words their definitions is maintained by a **dictionary**. 

AO is intended for use in a wiki-based programming environment, where each page is a word in the dictionary. An AO programming environment will make use of naming conventions: documentation, automatic tests, environment extensions, and active services may be identified based on prefix. A single dictionary can support thousands of projects with rich cross-project refactoring, flexible integration testing, and an ever more refined and reusable dictionary.

Most AO features come from ABC. See AboutABC for more information.

For bootstrapping, filesystem or command-line tools for AO will support a **.ao** dictionary format. This format has a simple patching-based import and dictionary model.


## Literals: Numbers, Text, Blocks

AO supports a range of useful number representations. By example:

        42         (integral)
        -12.3      (decimal)
        2.1e-3     (scientific; eN means *10^N)
        34.5%      (percentile; same as e-2)
        1/3        (rational)
        0xca7f00d  (hexadecimal)

In all cases, these are understood as exact rational numbers. Additionally, AO tags numbers with units, represented as a sorted list of `(dlabel * number)` pairs.

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

ABC code (see AboutABC) is inlined using pseudo-words, having prefix `%`. In addition, capabilities may be syntactically represented using `%{` to following `}`.

        %vrwlc      (aka `swap`)
        %lwcwrwc    (aka `rot4`)
        %{&par}     (an annotation)

The canonical expansion of inlined ABC is simply each ABC operator alone. For example, the definition of `%vrwlc` is effectively `%v %r %w %l %c`. Capabilities must always be in canonical form. AO's inlined ABC in AO may contain most of ABC. The exceptions are as follows: 

* no whitespace (LF, SP)
* no text or blocks
* no numbers (`#0123456789`)
* no semantic capabilities

Whitespace in ABC means identity. AO has its own support for text, numbers, and blocks. AO uses a dedicated reader state for capabilities. So the first three points don't hinder AO. The restriction on capabilities is discussed below.

*Note:* AO does not allow inlining of ABCD. ABCD extends ABC with a fixed dictionary, which is redundant with AO's own dictionary feature. ABC will instead compiled to ABCD as a postprocess, independently of how AO modularizes code.

## Proper Capability Security

AO prohibits syntactic representation of semantic capabilities. That is, most interesting capability text - e.g. to read or write a file, or to create a stateful object, or for high-performance matrix manipulations - may not be "hard wired" into the program. 

AO does allow annotations to be expressed using capability text. Annotations mostly express hints for a compiler, optimizer, debugger, e.g. to support parallelism, laziness, breakpoints, deprecation, typechecking. For example, `%{&par}` might apply to a block, and indicate the block should be evaluated in parallel.

Capabilities are usually shared via 'powerblock' - a block with a standard location in the environment, that can be asked for specific capabilities. This gives AO the feel of an ambient authority language, since full authority tends to be passed forward by default. AO programmers must instead be explicit about where they restrict authority, leveraging combinators that restrict authority in known ways:

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

*NOTES:* `(a)` is just `a`, and `(a|)` is an optional `a`. Ambiguous choice is associative, commutative, and idempotent. The order that choices are expressed has no impact on preference heuristics.

### Constraining Ambiguity

Search is expensive. Also, context-dependent meaning can be semantically troubling, e.g. it hinders equational reasoning. Ambiguity is a feature that must be used carefully, and removed from the codebase when it is no longer necessary.

One job of the programming environment is to help developers easily recognize and control where ambiguity is used. Towards this end, I suggest two techniques:

1. ambiguous words are colored or styled differently when rendered
2. automatic tests may introspect dictionary and fail if a word is ambiguous

When a word is ambiguous generally, but unambiguous in context, it might be rendered differently than if ambiguous in context. Overall, this design is simple, flexible, and easily extended for more attributes and properties. 

## Processing AO

### AO Definition Syntax

Parsing AO code is simple. AO code is a whitespace separated sequence of words, literals, and inlined ABC. Possibly a few ambiguous choices. The most difficult part is parsing numbers. AO currently needs special reader states for:

* numbers and units
* inline or block text
* capability text `%{` to following `}`
* blocks `[` ... `]`
* ambiguous structure `(`, `|`, `)`

Words in AO are very flexible in their structure. However, words are constrained to simplify parsing and printing. 

* words may not start with `%`, `-`, or a digit
* words may not contain `"`, `[`, `]`, `(`, `|`, `)`
* words may not contain C0 or C1 control characters, SP, or DEL.

A specific programming environment might have a few extra constraints, e.g. so words can be used in URLs. We may also unify or normalize some words, or may add a new class of pseudo-words. But most words should be allowed, including UTF-8.

Like ABC, AO uses only spaces and newlines for whitespace. 

### AO Dictionary File

AO is intended for a wiki-based programming environment. However, to help get started, AO defines a simple **.ao** dictionary file format - primarily for use with command-line tools. An **.ao** dictionary supports multiple definitions and imports of other dictionary files. The file format looks roughly like:

        imports before first definition
        @word1 definition1 using word2 word3
        @word2 definition2
        definitions can use multiple lines
        @word3 definition3

Regular entries start with `@word` at the beginning of a new line. The definition follows the word, up to the start of the next word. The initial character `@` is a separator, not part of the word. If a word is already defined, the earlier definition is replaced (retroactively). A word may be *undefined* by convention of defining a word to itself, e.g. `@foo foo`. 

The *import* section is special. Syntactically, it is a sequence of words. However, each word in the import list must identify an AO dictionary file (minus the **.ao** suffix) in a configurable search space. (I use current directory plus AO_PATH as search space.) Imports are applied in the order listed, left to right, loading words with later definitions replacing earlier ones. (This is trivial to optimize.)

Essentially, dictionary files and imports can be understood as concatenative functions that *patch* a tacit dictionary.

### Processing of AO Dictionary

Whether a dictionary develops in a wiki-based programming environment or an AO dictionary file, may be processed in many ways:

* detect cyclic definitions and raise errors
* detect definition of invalid words and raise errors
* detect use of undefined words and raise errors
* static analysis and typechecking for obvious errors
* generate AMBC or ABC code for any given word
* generate an 'optimized' dictionary
* leverage naming conventions for ad-hoc roles:
*   `test.foo` - automatic testing, more errors or warnings
*   `doc.foo` - automatic documentation or reports 
*   `app.foo` - automatic executable generation
*   `icon.foo` - generate an icon for a desktop app
*   programming environment extensions
*   live services - web services or publish/subscribe

An AO dictionary describes a suite or system with many kinds of outputs. 

AO does not have syntax for comments. Documentation is primarily achieved by defining documentation words. This greatly simplifies reuse, templates, frameworks, rich formatting with figures and graphs, automatic refactoring and projectional editors, interactive or hypertext documentation, and so on. A good AO programming environment should make documentation for a word readily accessible.

Testing in AO will include unit tests, but should also include [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck)-like property testing and some level of symbolic analysis and introspection. Tests also use mockup environments, which is easy to represent due to effects occurring through capabilities. Testing helps augment AO's gradual typing.

*Aside:* The centralized 'main' function of many languages seems to result in complications downstream - make systems, linkers, separate testing and document generation. My intuition is that AO's design is both simpler and more extensible. 

*Note:* An interesting possibility is to disassemble an ABC stream relative to a dictionary, i.e. treating a dictionary as an implicit grammar. AO dictionaries can often be understood as large grammars for extracting structure from a stream.

### Flat Namespace

Each word in the AO dictionary has a global definition, independent of local context. A single dictionary is expected to support thousands of projects. This design offers several advantages:

* eliminates local import/export boiler plate 
* common language, learning, and refactoring across projects
* word is module, function, and software component
* great fit for wiki-based programming environment
* opportunity for discovery, reuse, knowledge sharing
* leads naturally towards dense namespace, terse code

However, within a fresh project, framework, or DSL, we often wish to use words with minimal risk of conflict or collision. A flat namespace can be a disadvantage in this context. Developers will tend to use a prefix or suffix to distinguish the word, and this leads to increased verbosity, decreased readability.

Fortunately, we can address this weakness in a modern programming environment. An AO editor can be configured to recognize a prefix or suffix, and replace it at render with color or style. This information may be combined with styling for type, ambiguity, and so on.

I believe this feature can increase readability while reducing verbosity. Dependencies, relationships, and interesting content can be highlighted while first order structure and data shuffling fade into the background. Autocomplete can also render options in colored form, and perhaps be more [sublime](http://www.sublimetext.com/) with regards to quickly reaching options. Type-driven autocomplete is also an interesting possibility in AO.

Using a new prefix for each new project is okay. Do it. Refactor later.

## Standard Multi-Stack Environment

AO requires a basic `(s * e)` environment for literals. Beyond that, the expected environment depends on the dictionary and frameworks in use. However, changing requires widespread edits of the dictionary, so we want a decent extensible environment to start. I suggest:

        (stack * (hand * (powerblock * ((stackName * namedStacks) * ext))))

* stack - the current stack where operations occur
* hand - a second stack, used as a semantic clipboard
* named stacks - act as workspaces, registers, space for extensions area
* stack name - name of current stack, so we can switch workspaces
* powerblock - query for specific caps; source of state, authority, identity
* ext - unused, available for future extensions

A stack is modeled using pairs `(a * (b * (... * 1)))`.

So AO allows traditional stack-based programming on the main stack. However, the developer also has access to flexible data shuffling by use of the hand or named stacks. Also, multiple workspaces is very convenient when modeling complex workflows (one stack per task). We can take and put or copy and paste with the hand, store and load from named stacks, and switch the current workspace to a named stack.

Awelon project encourages compositional, widely reusable data structures: documents, diagrams, geometries, tables, matrices, grammars, constraint models, rulebooks, scene-graphs. Use of composition and lens or [zipper](http://en.wikibooks.org/wiki/Haskell/Zippers) based manipulations is encouraged. So the 'objects' on these stacks will often be document-like structures, and project-specific data structures are discouraged.

The powerblock is the generic entry point to observe or influence the real world. Specific capabilities can be extracted from a powerblock as required, and developers should do when deep enough know what *least authority* actually requires. Meanwhile, a powerblock can be forked into parent/child, where the child may be restricted and passed to a subprogram. 

