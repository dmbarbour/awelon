# Awelon Object Language (AO)

Awelon Object language (AO) is a programming language built above [Awelon Bytecode (ABC)](AboutABC.md). AO is a concatenative programming language (similar to Forth or Factor), but is distinguished from other such languages in several ways: 

* AO is not stack-based. It can operate on any value structured by pairs.
* AO leverages pairs to model multiple stacks, Huet zippers, lenses, tools.
* AO is capability based. There is no ambient authority for side-effects.
* AO is gradually typed. Supports static analysis and type inference.
* AO is structurally and substructurally typed; type model is compositional.
* AO exhibits *causal commutativity*, which enables implicit parallelism.
* AO exhibits *spatial idempotence*, which simplifies equational reasoning.
* AO can represent adaptive or declarative search-spaces of programs. 

In AO, a **word** is both a unit of modularity and a functional software component. A word has a definition. The relationship between words their definitions is maintained by a **dictionary** with a flat namespace. The formal semantics for every word is simply the inline expansion of its definition. Recursive definitions are invalid; loops are instead expressed using fixpoint combinators. Expansion ends at a finite sequence of text, numbers, blocks, and inlined ABC.

Words in AO additionally have *informal, extrinsic* semantics based on naming conventions. For example, words of form `doc.foo` represent documentation, and words of form `test.foo` can represent a suite of automated tests. Spreadsheet-like systems might be modeled within a dictionary using naming conventions like `a1$foo` and `b3$foo` to define cells that can be rendered together as spreadsheet 'foo'. (Interactive development in AO uses spreadsheet instead of REPL.) Conventional desktop and console apps may precipitate from a dictionary with each `app.xyzzy` word resulting in an 'xyzzy' executable.

Ultimately, an AO dictionary represents a complete system with hundreds of projects, services, and applications. The dictionary will evolve due to cross-project refactoring and integration testing. A good AO programming environment can augment AO and mitigate its weaknesses:

* style and color should replace rendering of frequent prefixes or suffixes
* hyperlinking or zooming to quickly access definitions and documentation
* automatic visualization of inferred stack-like structures, reduced burden
* graphical manipulation of rendered structures to simplify data shuffling
* automatic animation highlighting changes in structure across words in def
* support for automatic word completion, sensitive to type and context
* automated refactoring support; discover similar code in other projects
* live programming; continuous feedback of value over change in definition

In the absence of such an IDE, AO has a steep learning curve and is not very human-friendly. The **.ao** file format supports conventional filesystem and text editor environments, but is intended primarily for bootstrap development.

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

Blocks in AO use square brackets and contain arbitrary AO code. The square brackets qualify as word separators, so it's okay to run them together:

        [12.3 :foo dup bloop flip trip]
        [[[foo]each]keep]dip

While large blocks are permitted, large or deep blocks should generally be understood as a *code smell* - if you see a lot of them, it's wise to refactor. Consider use of [adverbs](doc/AboutAdverbs.md) or staged programming idioms.

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

AO compilers generally prohibit syntactic representation of semantic capabilities. That is, interesting capabilities should not be "hard wired" into program source code. Instead, they should be provided as arguments to the program, thus enabling secure reasoning about how authority is distributed through subprograms. 

Capabilities are usually shared via 'powerblock' - a block with a standard location in the environment, that can be asked for specific capabilities. This gives AO the feel of an ambient authority language, since full authority tends to be passed forward by default. AO programmers must instead be explicit about where they restrict authority, leveraging combinators that restrict authority in known ways:

        [trustMeHehHeh] runJailed

With a little convention, security implications should at least be visible and obvious in code, which is sufficient to achieve the principle of least authority when it most matters.

There are a few exceptions to the general rule. Developers are free to hard-code *annotations* and *discretionary sealers* using the inline ABC capability syntax. Some compilers might also accept secure hash sources.

### Annotations

Annotations potentially serve many roles - optimization, debugging, warnings, etc. (see [AboutABC](AboutABC.md)). The main restriction is that annotations cannot be *semantic* - i.e. they cannot impact formally observable behavior. If annotations are removed, performance characteristics might change but the formal results would not.

Annotations are described via capabilities using prefix `&` for the token. For example, `%{&par}` would be an annotation, potentially suggesting parallelization of a computation. AO allows arbitrary annotations in code. A compiler or interpreter will generally ignore annotations it doesn't recognize. 

### Discretionary Value Sealing

Value sealing with sealer/unsealer pairs is useful for many security patterns (see [AboutABC](AboutABC.md)). Even an insecure sealer can guard against much *accidental* behavior. 

Value sealing is a form of annotation in the sense that it doesn't have any observable semantics. That is, for a correct program, all sealer/unsealer pairs can be removed from the program without changing its behavior. Value sealing only causes some incorrect programs to fail or be rejected, and thus serves a role similar to 'newtype' in other languages.

Sealers and unsealers are represented as capabilities using inline ABC:

        %{:foo}       sealer 'foo' seals the value
        %{.foo}       unseal value from sealer 'foo'

In general, any sealed value must be treated as an opaque, atomic entity until unsealed. Only a few whole-value operations - in particular, copy and drop and quotation - are permitted if also allowed on the underlying value. 

## Processing AO

### AO Definition Syntax

Parsing AO code is simple. AO code is a whitespace (SP or LF) separated sequence of words, literals, and inlined ABC. Possibly a few ambiguous choices. The most difficult part is parsing numbers. AO currently needs special reader states for:

* numbers - decimal, fractional, hexadecimal
* text - inline or block
* capability text `%{` to following `}`
* blocks `[` ... `]`
* ambiguous structure `(`, `|`, `)`
* adverbs `\`

The latter two features are experimental. See [AboutAmbiguity.md](doc/AboutAmbiguity.md) and [AboutAdverbs.md](doc/AboutAdverbs.md). Words in AO are very flexible in their structure. However, words are constrained to simplify parsing, printing, quoting, and streaming. Also, block and amb characters work as word separators.

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

Regular entries start with `@word` at the beginning of a new line, followed by the definition. The initial `@` is an entry separator capable of isolating parse errors, and is not a valid word-start character. If a word is already defined, the earlier definition is replaced and a warning is issued (shouldn't happen silently). A word may be *undefined* by convention of placing it in a cycle with itself, e.g. `@foo foo`. 

The *import* section, before the first entry, is special. Syntactically, it is a space-separated sequence (where 'space' means SP or LF). Words from each import are loaded in order, albeit optimized to silently eliminate redundant loads or cycles. Imports are currenly located by simply searching the `AO_PATH` environment variable for the first file named the same as the import plus a **.ao** suffix. 

A complete AO dictionary is generally specified from an initial import.

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
