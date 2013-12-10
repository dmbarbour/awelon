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
        2/3        (rational)
        0xca7f00d  (hexadecimal)

In all cases, these are understood as exact rational numbers. Additionally, AO uniformly pairs numbers with units, represented as a sorted list of `(label * number)` pairs. A number literal without units will simply result in an empty list of units, representing a scalar entity.

        1.4e2`kg*m/s^2
        1/3`apple
        -12.3`C

Units for numbers are very useful for static safety validation, and for keeping some context. However, beyond providing some standard structure, AO leaves interpretation or normalization of units to user code. The unit expression is assumed to be of the form `x*y/a*b`, allowing for `1/a`, or `m^N`. One `/` character is allowed, placing everything to its right in the denominator. 

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

The canonical expansion of inlined ABC is simply each ABC operator alone. For example, the definition of `%vrwlc` is effectively `%v %r %w %l %c`. Capabilities must always be in canonical form. 

AO's inlined ABC in AO may contain most of ABC, excepting text, numbers (`#0123456789`), blocks, and whitespace. Of course, AO has its own support for text, numbers, blocks, and whitespace. In addition, while AO is syntactically able to represent any ABC capability, most AO compilers will forbid all except annotations. The restriction on capabilities is discussed below.

*Note:* AO does not allow inlining of ABCD. ABCD extends ABC with a fixed dictionary, which is redundant in context of AO's own dictionary feature. Instead, ABCD should always be generated as a postprocess to compress a raw ABC stream.

## Proper Capability Security

AO compilers should prohibit syntactic representation of semantic capabilities. That is, most interesting capabilities should not be "hard wired" into a program. Instead, they should be provided as arguments to the program, thus enabling secure reasoning about how authority is distributed to subprograms. 

Capabilities are usually shared via 'powerblock' - a block with a standard location in the environment, that can be asked for specific capabilities. This gives AO the feel of an ambient authority language, since full authority tends to be passed forward by default. AO programmers must instead be explicit about where they restrict authority, leveraging combinators that restrict authority in known ways:

        [trustMeHehHeh] runJailed

With a little convention, security implications should at least be visible and obvious in code, which is sufficient to achieve the principle of least authority when it most matters. 

Annotations - by convention, capabilities with prefix `&` - are not semantic and may be hard-wired into an application. Annotations express hints for a compiler, optimizer, debugger, e.g. to support parallelism, laziness, breakpoints, better warning or error messages, and so on. For example, `%{&par}` might apply to a lazy thunk and indicate its value should be calculated in parallel.

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

Many expansions can be eliminated if they are not *meaningful*, that is if they are not type safe in context or use undefined words. Of the valid expansions, one will be chosen heuristically. That is, rather than making a random choice, we search for a valid program that has nice characteristics and qualities according to a developer or configuration. 

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

Parsing AO code is simple. AO code is a whitespace (SP or LF) separated sequence of words, literals, and inlined ABC. Possibly a few ambiguous choices. The most difficult part is parsing numbers. AO currently needs special reader states for:

* numbers and units
* inline or block text
* capability text `%{` to following `}`
* blocks `[` ... `]`
* ambiguous structure `(`, `|`, `)`

Words in AO are very flexible in their structure. However, words are slightly constrained to simplify reading, parsing, printing, and streaming. Also, block and amb characters work as word separators.

* words may not start with `@`, `%`, `-`, or a digit
* words may not contain `"`, `[`, `]`, `(`, `|`, `)`
* words may not contain C0 or C1 control characters, SP, or DEL.

A specific programming environment might have a few extra constraints, e.g. so words can be used in URLs. We may also unify or normalize some words, or may add a new class of pseudo-words. But most words should be allowed, including UTF-8.

### AO Dictionary File

AO is intended for a wiki-based programming environment. However, to help get started, AO defines a simple **.ao** dictionary file format - primarily for use with command-line tools. An **.ao** dictionary supports multiple definitions and imports of other dictionary files. This dictionary format looks like:

        import list before first definition
        @word1 definition1 using word2 word3
        @word2
        definitions may use 
        multiple lines
        @word3 [definition3]

Regular entries start with `@word` at the beginning of a new line, followed by the definition. The initial `@` is not part of the word, but is an entry separator capable of isolating parse errors. If an entry doesn't parse, it is ignored with a warning. If a word is already defined, the earlier definition is replaced. A word may also be *undefined* by convention of defining a word to itself, e.g. `@foo foo`. 

The *import* section, before the first entry, is special. Syntactically, it is a space-separated sequence (where 'space' means SP or LF). Each element identifies an AO dictionary file, minus the **.ao** suffix. The search is configured via the `AO_PATH` environment variable. Missing, ambiguous, or cyclic imports result in error. Imports are processed sequentially from left to right, potentially replacing earlier definitions. 

Replacing definitions has global scope. Entries and imports may thus be understood as *patches* on a tacit dictionary. Only the final dictionary is evaluated. Hence, it is acceptable for a dictionary to temporarily contain errors, undefined words, or cycles.

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
*   live services: web services, publish/subscribe, control systems

By leveraging naming conventions to decide processing of words, a single AO dictionary can describe a whole system of services, applications, documents, tests, configurations, plugins or extensions (via capability secure reflection on a dictionary), and other outputs. 

AO does not have syntax for comments. Instead, define documentation words. In general, each word may be associated with a documentation word by naming convention. These words can describe rich structure - templates, formatting, figures and graphs, potentially even interactive instruction. A good AO programming environment should make documentation readily accessible. 

Tests in AO include unit tests, [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck)-style property testing, and deep reflective analysis on the dictionary. The capability-secure nature of AO can help with modeling mockup environments and econfigurations.

*Aside:* The singular 'main' function of mainstream languages is a significant source of accidental complexity. Developers are forced to use external make systems and linkers to configure multiple applications. Further, applications are not reusable as software components. AO's flexible use of naming conventions should mitigate these issues.

*Note:* An interesting possibility is to disassemble an ABC stream relative to a dictionary, i.e. treating a dictionary as an implicit grammar. AO dictionaries can often be understood as large grammars for extracting structure from a stream.

### Interactive AO

My vision for interactive AO involves live maintenance of a dictionary. A significant difference from a conventional REPL is that there is no implicit environment passed from one step to the next. Small, sequential steps may be modeled by having new definitions begin by naming the previously defined word, e.g.:

        @a1 3       -- renders '3'
        @a2 a1 4 +  -- renders '7'
        @a3 a2 6 *  -- renders '42'

Common patterns like this should, of course, be readily supported by the environment. This interaction may be 'live' in the sense that, at any time, we can update a definition and see changes propagate.

        @a1 5        -- renders 5; a2 renders 9; a3 renders 54

In some ways, interactive AO may be close in nature to a spreadsheet, at least for programs of type `[1→(x*1)]`. With appropriate naming conventions, even rendering as a spreadsheet is feasible. Of course, as mentioned before, different names may also be understood (by naming convention) as specifying tests, documentation, data, configurations, services, extensions, or applications. 

### Flat Namespace

Each word in the AO dictionary has a global definition independent of local context. A single dictionary is expected to support thousands of projects. This design offers several advantages:

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

## Multi-Stack Environment

Just as developers operate on a tacit dictionary, AO words and literals operate on a tacit value. The latter value is structured and can often be understood as modeling an 'environment' for computation - e.g. a stack, or multiple stacks. The structure of this environment is determined by convention. However, changes are expensive, requiring widespread edits to data shuffling words.

Based on a few experiments, I recommend the following as a flexible starting model for most AO systems:

        (stack * (hand * (power * ((stackName * namedStacks) * ext)))))

* stack - the current stack where operations occur
* hand - a second stack, used as a semantic clipboard
* named stacks - act as workspaces, registers, space for extensions area
* stack name - name of current stack, so we can switch workspaces
* power - block; query for specific caps; source of state, authority, identity
* ext - unused, available for future extensions

A stack is modeled using pairs, e.g. `(a * (b * (... * 1)))`.

Traditional stack-based programming occurs on the current stack. The extra named stacks can model registers, inventories, or extra workspaces. Multiple workspaces are very convenient when modeling complex concurrent workflows - i.e. *one stack per task*. The hand enables take, put, copy, paste, and provides a very convenient temporary storage.

I also recommend that objects on the stack typically be composable structures (documents, diagrams, geometries, tables, matrices, grammars, constraint models, rulebooks, scene-graphs, etc.) or mechanisms to surgically access and manipulate deep structure (e.g. [zippers](http://en.wikibooks.org/wiki/Haskell/Zippers) or [lenses](http://www.cis.upenn.edu/~bcpierce/papers/lenses-etapsslides.pdf)).

The powerblock serves as the general-purpose entry point to observe or influence the real world. Specific capabilities can be extracted from a powerblock as required, and developers should use specific capabilities when deep enough know what *least authority* actually requires. Meanwhile, a powerblock may be forked such that a child - granted to an distrusted subprogram - is restricted based on upstream policies. 

