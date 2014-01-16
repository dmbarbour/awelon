# Awelon Object Language (AO)

Awelon Object language (AO) is a programming language built above [Awelon Bytecode (ABC)](AboutABC.md). AO is a concatenative programming language, but is distinguished from other such languages in several ways: 

* AO is not stack-based. It can operate on any value structured by pairs.
* AO leverages pairs to model multiple stacks, Huet zippers, lenses, tools.
* AO is capability based. There is no ambient authority for side-effects.
* AO is gradually typed. Supports static analysis and type inference.
* AO is structurally and substructurally typed; type model is compositional.
* AO exhibits *causal commutativity*, which enables implicit parallelism.
* AO exhibits *spatial idempotence*, which simplifies equational reasoning.
* AO can represent adaptive or declarative search-spaces of programs. 

In AO, a **word** is both a unit of modularity and a functional software component. A word has a definition. The relationship between words their definitions is maintained by a **dictionary** with a flat namespace.

The formal semantics for every word is simply the inline expansion of its definition. Recursive definitions are invalid; loops are instead expressed using fixpoint combinators. Expansion ends at a finite sequence of text, numbers, blocks, and inlined ABC.

Words in AO additionally have *informal* semantics based on naming conventions. For example, words of form `doc.foo` represent documentation, and words of form `test.foo` can represent a suite of automated tests. Spreadsheet-like systems can be modeled within a dictionary using naming conventions like `a1$foo` and `b3$foo` to define cells rendered together as spreadsheet 'foo'. (Interactive development in AO uses spreadsheet instead of REPL.) Conventional desktop and console apps may precipitate from a dictionary with each `app.xyzzy` word resulting in an 'xyzzy' executable.

Ultimately, an AO dictionary represents a complete system with hundreds of projects, services, and applications. The dictionary will evolve due to cross-project refactoring and integration testing. 

A good AO programming environment can augment AO or mitigate its weaknesses:

* style and color should replace rendering of common prefixes or suffixes
* hyperlinking or zooming to quickly access definitions and documentation
* automatic visualization of inferred stack-like structures, reduced burden
* graphical manipulation of rendered structures to simplify data shuffling
* automatic animation highlighting changes in structure across words in def
* support for automatic word completion, sensitive to type and context
* automated refactoring support; discover similar code in other projects

AO is envisioned with these features in a wiki-based programming environment. However, a simple **.ao** dictionary file format with imports has been defined for a more conventional filesystem and text-editor programming environment. Working without automatic visualization has a steep learning curve that can intimidate potential programmers, so I do not wish to promote the **.ao** dictionary format much beyond its intended use for bootstrapping.

## Literals: Numbers, Text, Blocks

AO supports a range of useful number representations. By example:

        42         (integral)
        -12.3      (decimal)
        2.1e-3     (scientific; eN means *10^N)
        34.5%      (percentile; same as e-2)
        2/3        (rational)
        0xca7f00d  (hexadecimal)

In addition, developers are strongly encouraged to augment numbers with unit information. By convention, words for units are prefixed with a backquote and have type `num → (num * dim)` pair (for the top object on the current stack). The type for dimensions is typically a sorted list of `(label * num)` pairs.

        1.4e2 `kg*m/s^2
        1.4e2 `Joules
        -12.3 `C

Units are useful for static safety validations, and they provide a little extra context and meaning for the numbers. AO math words will generally include logic to recognize and process units. 

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

## Ambiguity and Program Search

Ambiguity is syntactically expressed by wrapping a section in parentheses, and separating one or more options with a vertical bar. For example:

        a (b | c d) e (f|g|h)

The meaning of the above subprogram may be any one of:

        a b e f
        a b e g
        a b e h
        a c d e f
        a c d e g
        a c d e h

The choice of meanings in a given use case is left to the AO compiler. Formally, the choice is non-deterministic, but it is not random. Similar to ambiguity in natural language, ambiguity in AO is *resolved in context*.  Choices are eliminated if obviously not typesafe in context. The remaining choices may be searched based on a heuristic functions, which may evaluate options for performance, size, confidence of safety, stability (across versions of a program), and programmer attribute annotations.

        %{&attrib} :: (Attribute x) => (x * e) -> (x * e)

Through attributes and control of the heuristic function, programmers can effectively influence the compiler's choice. However, there is never a guarantee that an optimum solution will be selected. It is not difficult to express programs with a hundred options for 2^100 meanings or more. With such large search spaces, non-exhaustive mechanisms must be used to select a 'good' program - e.g. hill climbing or genetic programming. 

The choice operator `|` is *commutative, associative, and idempotent*. The syntactic order in which choices are expressed must not contribute to heuristic evaluation of choices. This independence is important for refactoring ambiguous programs, and for optimizing search. It also means that, formally, we can understand ambiguity as expressing a *set* of programs. 

Ambiguous code has many roles: rapid prototyping, self-healing or adaptive code, exploring design spaces and tradeoffs, and tactical theorem proving. Adding or removing ambiguity allows programmers to shift gradually between 'crystalline' code with rigid structure and 'fluid' code that can reshape itself for its context. Developers can control where and how much adaptation occurs.

### Constraining Ambiguity

Context-dependent or non-deterministic meaning can be semantically troubling. For example, it hinders equational reasoning. Also, search is expensive. Ambiguity should be avoided or removed from a codebase when there is no obvious benefit from it. Where feasible, we should push program search to edit-time, and perhaps resolve ambiguous code at edit-time. 

A good programming environment can help developers manage ambiguity:

1. ambiguous words are styled or colored differently when rendered
2. automatic tests may be set to fail if specific code is ambiguous

The first technique helps developers recognize ambiguity without digging deeply through code. The second technique helps control ambiguity, preventing it from stealthily entering the dictionary.

## Processing AO

### AO Definition Syntax

Parsing AO code is simple. AO code is a whitespace (SP or LF) separated sequence of words, literals, and inlined ABC. Possibly a few ambiguous choices. The most difficult part is parsing numbers. AO currently needs special reader states for:

* numbers - decimal, fractional, hexadecimal
* text - inline or block
* capability text `%{` to following `}`
* blocks `[` ... `]`
* ambiguous structure `(`, `|`, `)`
* adverbs `\` (proposal, discussed later)

Words in AO are very flexible in their structure. However, words are constrained to simplify parsing, printing, quoting, and streaming. Also, block and amb characters work as word separators.

* words may not start with `@`, `%`, `-`, or a digit
* words may not contain `"`, `[`, `]`, `(`, `|`, `)`, or `\`
* words may not contain C0 or C1 control characters, SP, or DEL.

A specific programming environment might have a few extra constraints, e.g. so words can be used in URLs. We may also unify or normalize some words, or may add a new class of pseudo-words. But most words should be allowed, including UTF-8.

### AO Dictionary File

To help bootstrap, AO defines a simple **.ao** dictionary file format - primarily for use with command-line tools. An **.ao** dictionary supports multiple definitions and imports of other dictionary files. This dictionary format looks like:

        import list before first definition
        @word1 definition1 using word2 word3
        @word2
        definitions may use 
        multiple lines
        @word3 [definition3]

Regular entries start with `@word` at the beginning of a new line, followed by the definition. The initial `@` is an entry separator capable of isolating parse errors, not part of any word. If a word is already defined, the earlier definition is replaced and a warning is issued (shouldn't happen silently). A word may be *undefined* by convention of placing it in a cycle with itself, e.g. `@foo foo`. 

The *import* section, before the first entry, is special. Syntactically, it is a space-separated sequence (where 'space' means SP or LF). Imports are loaded into the dictionary sequentially from left to right, replacing earlier definitions - trivially optimized to eliminate redundant processing. Imports are currenly located by searching the `AO_PATH` environment variable for a file named the same as the import plus a **.ao** suffix. Missing, cyclic, or ambiguous imports result in error.

### Processing of AO Dictionary

Independently of how a dictionary is maintained, it may be processed in several ways:

* detect cyclic definitions and raise errors
* detect invalid definitions and parse errors 
* detect use of undefined words and raise errors
* static analysis and typechecking for obvious errors
* on demand, compile a word to AMBC or ABC
* leverage ad-hoc naming conventions for ad-hoc roles
*   `test.foo` - automatic testing, more errors or warnings
*   `doc.foo` - automatic documentation or reports 
*   `app.foo` - automatic executable generation
*   `b3$foo` - word as cell in the 'foo' spreadsheet
*   programming environment extensions or configuration variables
*   live services: web services, publish/subscribe, control systems

By leveraging naming conventions to decide processing of words, a single AO dictionary can describe a whole system of services, applications, documents, tests, configurations, plugins or extensions (via capability secure reflection on a dictionary), and other outputs. In a suitable context, an AO dictionary can act much like the filesystem of a larger operating system.

AO does not have syntax for comments. Instead, developers must define documentation words. In general, each word may be associated with a documentation word through naming conventions. These words can describe rich structure - templates, formatting, figures and graphs, potentially even interactive instruction. A good AO programming environment should make documentation readily accessible. 

Tests in AO include unit tests, [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck)-style property testing, and deep reflective analysis on the dictionary (via reflective capabilities). The capability-secure nature of AO can help with modeling mockup environments and configurations.

*Aside:* The singular 'main' function of mainstream languages is a significant source of accidental complexity. Developers are forced to use external make systems and linkers to configure multiple applications. Further, applications are not reusable as software components. AO's flexible use of naming conventions should mitigate these issues.

*Note:* An interesting possibility is to disassemble an ABC stream relative to a dictionary, i.e. treating a dictionary as an implicit grammar. AO dictionaries can often be understood as large grammars for extracting structure from a stream.

### Interactive AO

My vision for interactive AO is closer in nature to a spreadsheet than a REPL. Developers manipulate definitions for a small, structured subset of dictionary words. A proposed naming convention is `a1$foo` and `b3$foo` naming cells that can be rendered together as spreadsheet 'foo'. Rendering may also hide a rendundant `$foo` suffix, instead displaying `a1` or `b3` with a configurable color. 

A REPL can trivially be modeled in a spreadsheet by treating each command as sequentially defining a row in the spreadsheet. To represent a continuing session with lots of steps, each word simply starts with the previous word (leveraging a feature of concatenative programming). For example:

        @a1 3       -- renders 3
        @a2 a1 4 +  -- renders 7
        @a3 a2 6 *  -- renders 42

Of course, unlike traditional REPLs, one might redefine a word at any time.

        @a1 5       -- renders 5; a2 renders 9; a3 renders 54

A good AO programming environment should provide support for viewing 'live' spreadsheets, where cells in the spreadsheet may use any word from the dictionary - including other spreadsheets modeled in the same dictionary. Such spreadsheets can include information about tests, and basically provide some health information about the dictionary overall.

*Aside:* Rendering for cells with simple types like `[1→x]` is obvious. However, Conal Elliott's work on [tangible values](http://conal.net/papers/Eros/) suggests that many functions may be usefully rendered. Developers can be given control by specifying a rendering context for common views of the spreadsheet, such that each cell `b3$foo` renders as `[b3$foo] render`. 

### Flat Namespace

Each word in the AO dictionary has a global definition independent of local context. A single dictionary is expected to support thousands of projects. This design offers several advantages:

* eliminates local import/export boiler plate 
* common language, learning, and refactoring across projects
* word is module, function, and software component
* great fit for wiki-based programming environment
* opportunity for discovery, reuse, knowledge sharing
* leads naturally towards dense namespace, terse code

Flat namespaces have one great, classical and well known weakness. Risk-averse developers will tend to use long disambiguating words, i.e. including the name of the project, framework, library, or DSL. This leads to phrases such as `foo.projectQux bar.projectQux baz.projectQux` that are verbose and almost intolerable to read or write. 

Fortunately, we can mitigate *or even reverse* this weakness in context of a modern development environment. An AO editor can recognize common prefixes or suffixes and hide them on render, instead disambiguating by user-configurable styles and colors. Similarly, on edit, auto-complete features with fuzzy find can simplify discovery and use of long words.

Developers are thus free to use a new suffix for each project or framework as a pseudo-namespace. When functionality proves to be more widely useful, it can later be refactored into a more generic space.

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

## Expression Problem and Policy Injection

AO will be pursuing a new, experimental alternative to the configurations problem that supports default implementations, soft constraints, heuristic policy injection, and deep overrides. The idea is to leverage dependent types, partial evaluation, and staged constraint solvers to automate a lot of glue-code. 

This is low priority at the moment, but it will eventually have a pervasive impact on the AO programming experience. Relevantly, it will serve roles similar to typeclasses and dependency injection frameworks.

## Adverbs (EXPERIMENTAL!)

In AO, we might decide to apply some word `foo` to each element of a list. We can easily express this as `[foo] each`, which would directly apply foo to each element of a list. If we further want to keep a copy of the list, we might modify this to `[[foo] each] keep`. If we also want to hide the first element on the stack, we might modify this to `[[[foo] each] keep] dip`.

An 'adverb' is a word that modifies a verb. 

Words such as `each`, `keep`, and `dip` aren't adverbs. They're too active. But they are at least *related* to adverbs. If we were instead to say `[foo] listwise`, we might expect as result a function - a verb - that, *when later applied to a list*, will apply `foo` to each element in the list. We could define `listwise` as simply `[each] curry`.

Adverbs have a nice property: they operate on a closed set of verbs. This makes them very compositional in nature, and a good fit for concatenative PLs. We can meaningfully say `[foo] listwise barwise bazwise`, and we can readily refactor or abstract common sequences of adverbs. 

Unfortunately, `[foo] each` is simply easier to write than `[foo] listwise inline`, even if we ignore the one-time cost to define `listwise`. At least with respect to this pattern, the path of least resistance guides developers to an inferior solution. What I propose here is syntactic sugar for adverbs in AO to shift parsimony in favor of adverbs. To get a sense of the proposed sugar: instead of `[[[foo] each] keep] dip`, we might write `foo\*kd`.

For this sugar:

* the character `\` is now reserved, may not be used in words
* users define adverbs as special words of format `\k` or `\*`
* each adverb is distinguished by only a single character
* like inline ABC, `\adverbs` expands to `\a \d \v \e \r \b \s`.
* adverbs often directly modify a word, such as `foo\adverbs`
* `foo\adverbs` expands to `[foo] [\adverbs] .apply inline`

A couple points: First, this sugar limits developers to a finite vocabulary of adverbs. In practice, this won't be a problem: a few dozen adverbs should cover the vast majority of use-cases. Also, the the desugared form is still available for uncommon adverbs. Second, adverbs are applied in a constrained environment. By `.apply` and `inline` I mean compiler-defined operations, but the idea is that adverbs should neither observe nor influence the context in which they are applied. 

At the moment, the adverbs sugar is an experiment. But if they prove popular, they will be retained.

