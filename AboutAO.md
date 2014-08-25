# Awelon Object Language (AO)

Awelon Object language (AO) is a programming language built (very thinly) above [Awelon Bytecode (ABC)](AboutABC.md). AO is a concatenative programming language similar to Forth, Factor, Joy, or Cat, but is distinguished from these Forth-like languages in several ways: 

* AO is not stack-based. It can operate on any value structured by pairs.
* AO leverages pairs to model multiple stacks, Huet zippers, lenses, tools.
* AO is capability based. There is no ambient authority for side-effects.
* AO is gradually typed. Supports static analysis and type inference.
* AO is structurally and substructurally typed; type model is compositional.
* AO exhibits *causal commutativity*, which enables implicit parallelism.
* AO exhibits *spatial idempotence*, which simplifies equational reasoning.
* AO can represent adaptive or declarative search-spaces of programs. 

In AO, a **word** is both a unit of modularity and a functional software component. A word has a definition. The relationship between words their definitions is maintained by a **dictionary** with a flat namespace. The formal semantics for every word is simply the inline expansion of its definition. Recursive definitions are invalid; loops are instead expressed using fixpoint combinators. Expansion ends at a finite sequence of text, numbers, blocks, and inlined ABC.

Words in AO additionally have *informal, extrinsic* semantics based on naming conventions. For example, words of form `doc.foo` represent documentation, and words of form `test.foo` can represent a suite of automated tests. Conventions may guide use of word-level separate compilation and linking. The vision for AO is a growing, organic dictionary, with a hundred thousand words for hundreds of projects, tests, documents, services, and software components. Some parts of the dictionary will calcify, while others are subject to large scale refactorings across projects.

The **.ao** file format supports conventional filesystem and text editor environments, but is intended primarily for development prior to richer environments.

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

By convention and role, we call the value `s` the stack, and `e` the environment. Literals are added to the stack, but the environment remains accessible. Translation from AO to ABC is trivial: `%l` is implicitly introduced after every literal in AO. AO developers may express ABC behavior with simple bracketing: `%v 42 %c :: e → (42 * e)`. But doing so is inconvenient and uncommon in practice; instead, just assume the AO stack while working within AO. The codes `%l`, `%v`, and `%c` are examples of inline ABC. 

## Inline ABC

ABC code (see AboutABC) is inlined using pseudo-words, having prefix `%`. In addition, capabilities may be syntactically represented using `{` to following `}`.

        %vrwlc      ((a*b)→(b*a); a primitive swap)
        %rwrzw$l    (x [x→y] -- y; apply function to value)
        {&asynch}   (an annotation for parallelism)
        {&static}   (annotation for compile-time evaluation)

The canonical expansion of inlined ABC is simply each ABC operator alone. For example, the expansion of `%vrwlc` is equivalent to `%v %r %w %l %c`. ABC's drop operator would be represented as `%%`.

AO's inlined ABC in AO may contain most of ABC, excepting text, numbers (`#0123456789`), blocks, and whitespace. AO has its own support for text, numbers, blocks, and whitespace. In addition, while AO is syntactically able to represent any ABC capability, most AO compilers should forbid all except annotations and discretionary sealers. This restriction is valuable for proper capability security.

*Note:* AO developers cannot directly utilize ABCD or ABC's separately compiled resources. However, words may be configured indirectly for compilation, and thus may implicitly be reduced to ABCD or separately compiled resources.

## Proper Capability Security

AO compilers generally prohibit syntactic representation of semantic capabilities. That is, interesting capabilities should not be "hard wired" into program source code. Instead, they should be provided as arguments to the program, thus enabling secure reasoning about how authority is distributed through subprograms. 

Capabilities are usually shared via 'powerblock' - a block with a standard location in the environment, that can be asked for specific capabilities. This gives AO the feel of an ambient authority language, since full authority tends to be passed forward by default. 

AO programmers must instead be explicit about where they restrict authority, leveraging combinators that restrict authority in known ways. For example, `apply3to2` will execute a block in a simplified environment using the first three items on the stack, and resulting in two items on the stack:

        [trustMeHehHeh] apply3to2

With a little convention, security implications should at least be visible and obvious in code, which is sufficient to achieve the principle of least authority when it most matters.

There are a few exceptions to the general rule. Developers are free to hard-code *annotations* and *discretionary sealers* using the inline ABC capability syntax. Some compilers might also accept secure hash sources.

### Annotations

Annotations potentially serve many roles - optimization, debugging, warnings, etc. (see [AboutABC](AboutABC.md)). The main restriction is that annotations cannot be *semantic* - i.e. they cannot impact formally observable behavior. If annotations are removed, performance characteristics might change but the formal results would not.

Annotations are described via capabilities using prefix `&` for the token. For example, `{&par}` would be an annotation, potentially suggesting parallelization of a computation. AO allows arbitrary annotations in code. A compiler or interpreter will generally ignore annotations it doesn't recognize. 

### Discretionary Value Sealing

Value sealing with sealer/unsealer pairs is useful for many security patterns (see [AboutABC](AboutABC.md)). Even an insecure sealer can guard against much *accidental* behavior, and thus serve a role similar to 'newtype' in other languages.

Value sealing is a form of annotation in the sense that it doesn't have any observable semantics. That is, for a correct program, all sealer/unsealer pairs can be removed from the program without changing its behavior. Value sealing only causes some incorrect programs to fail or be rejected.

Sealers and unsealers are represented in AO as invocations:

        {:foo}      sealer 'foo' seals the value
        {.foo}      unseal value from sealer 'foo'

In general, any sealed value must be treated as an opaque, atomic entity until unsealed. Only a few whole-value operations - in particular, copy and drop and quotation - are permitted if also allowed on the underlying value. 

For true security, developers must generate sealer/unsealer pairs effectfully, via powerblock or related capability. 

## Processing AO

### AO Definition Syntax

Parsing AO code is simple. AO code is a whitespace (SP or LF) separated sequence of words, literals, explicit ambiguous structure, and inlined ABC. The most difficult part is parsing numbers. AO currently needs special reader states for:

* numbers - decimal, fractional, hexadecimal, negatives
* text - inline or block
* inline ABC, e.g. `%vrwlc`
* capability text `{` to following `}`
* blocks `[` ... `]`

In addition, there are some experimental extensions to AO under evaluation. An [ambiguity](doc/AboutAmbiguity.md) extension uses `(`, `|`, and `)`. A concept for [embedded literal objects](doc/ExtensibleLiteralTypes.md) uses `〚` and `〛` (U+301A-B). I'm further reserving the other unicode white variants `⦃⦄⦅⦆` (U+2983-6) for potential future extensions and experiments.

Most of UTF-8 is available to define words. However, words are constrained to simplify reading, parsing, printing, documenting, quoting, streaming, and extending. So the following limits apply, albeit roughly:

* words may not start with `@`, `%`, or a digit
* words may not contain `"[]{}`, \`, or extension chars `(|)⦃⦄⦅⦆〚〛`
* words may not contain C0 or C1 control characters, SP, or DEL
* words starting with `+`, `-`, or `.` may not follow with a digit

If developers insist on whitespace within their words (e.g. as opposed to underscores or hyphens), they should use NBSP (U+00A0). 

In addition to white space (SP, LF), block characters `[` and `]` act as word separators. The structure of a word is not interpreted within AO, though it may suggest a contextual meaning (e.g. `test.foo` becomes an automatic test) - see Processing, below.

### AO Dictionary File

To support *early* development using filesystem and text editor, AO has a simple **.ao** dictionary file format - primarily for use with command-line tools. Each **.ao** dictionary file supports multiple definitions plus imports of other dictionary files. This format looks like:

        import1 import2 import3
        @word1 definition1 using word2 word3
        @word2
        "some definitions require multiple lines
         but one line definitions are good style
        ~ 
        @word3 uses a [block]

Regular entries start with `@word` at the beginning of a new line, followed by the definition. The initial `@` is an entry separator capable of isolating parse errors, and is not a valid word-start character. If a word is already defined, the earlier definition is replaced and a warning is issued. Cyclic definitions are an error and are removed from the dictionary.

The *import* section, before the first entry separator, is special. Syntactically, it is a space-separated sequence (where 'space' means SP or LF). Imports are located by searching the `AO_PATH` environment variable (with implicit **.ao** suffix). Each import file is processed once, regardless of duplication or cycles, favoring an order such that those listed last are loaded last. A dictionary as a whole is specified from a root file or text.

This format will be deprecated in the near future. It doesn't scale well, and doesn't readily support rich tooling (refactoring, real-time type information, etc.). AO dictionaries will eventually move to programmatically rich, persistent storage media like databases.

### Processing of AO Dictionary

Independently of how a dictionary is maintained, it may be processed in several ways:

* detect cyclic definitions and raise errors
* detect invalid definitions and parse errors 
* detect use of undefined words and raise errors
* static analysis and typechecking for obvious errors
* on demand, compile a word to AMBC or ABC
* leverage ad-hoc naming conventions, e.g.
*   `test.foo` - automatic testing; emit errors or warnings
*   `eqv.foo` - declare/assert equivalency between subprograms
*   `doc.foo` - automatic documentation or reports 
*   `app.foo` - automatic executable generation
*   `b3$foo` - word as cell `b3` in the `foo` spreadsheet
*   `compile!foo` - suggest AO system separately compile `foo`
*   `secret!foo`  - indicate that `foo` contains sensitive information
*   programming environment extensions or configuration variables
*   live services: web services, publish/subscribe, control systems

By leveraging naming conventions to determine external processing of words, a single AO dictionary can describe a whole system of services, applications, documents, tests, configurations, plugins or extensions (via capability secure reflection on a dictionary), and other outputs. In a suitable context, an AO dictionary can act much like the filesystem of a larger operating system, with words containing data or processes. (This would be an interesting direction to explore, OS as a dictionary in a PL.)

AO does not have syntax for comments. Instead, developers must define documentation words. In general, each word may be associated with a documentation word through naming conventions. These words can describe rich structure - templates, formatting, figures and graphs, potentially even interactive instruction. A good AO programming environment should make documentation readily accessible.

Tests in AO potentially include unit tests, [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck)-style property testing, and deep reflective analysis on the dictionary (via reflective capabilities). The capability-secure nature of AO can help with modeling mockup environments and configurations.

*Aside:* The singular 'main' function of mainstream languages is a significant source of accidental complexity. Developers are forced to use external make systems and linkers to configure multiple applications. Further, applications are not reusable as software components. AO's flexible use of naming conventions should mitigate these issues; e.g. by use of an `app.` prefix, a single dictionary can describe any number of 'applications' with distinct names and shared structure.

*Note:* An interesting possibility is to disassemble an ABC stream relative to a dictionary, i.e. treating a dictionary as an implicit grammar. In some sense, AO dictionaries can be understood as large grammars for extracting structure from a stream. The challenge here is that an ABC stream will often have already been partially evaluated and optimized, and so will not correspond directly to most dictionaries.

### Interactive AO

The REPL `aoi` has been implemented for AO as part of the Haskell implementation. This operates in a conventional manner: developers can write some code, numbers, and text on a line. A summary of the stack is printed between commands. Developers cannot define new words in the REPL, but may modify and reload the dictionary on-the-fly. 

However, my vision for AO development is closer in nature to spreadsheets than REPLs, leveraging tests for automatic display, and actually working in spreadsheet-like environments for developing difficult algorithms where a lot of internal debugging is useful. A proposed naming convention for spreadsheets is `a1$foo` and `b3$foo`, specifying cells that can be rendered together as a spreadsheet `foo`. When such words are rendered, the redundant `$foo` suffix could be hidden, instead displaying `a1` or `b3` with a configurable color. (Of course, `a1$foo` and the like would remain accessible as plain words in other contexts, including other spreadsheets.)

Spreadsheets generally contain a single value per cell. The same is true in AO, except in AO it is also convention for said value to represent a full stack or multi-stack environment (see below). And such a stack might contain complex values modeling documents or geometries or scene-graphs. An AO spreadsheet could be much richer than conventional spreadsheets.

It is not difficult to model a REPL above a spreadsheet metaphor: simply use a linear sequence of cells (one for each step in the REPL). This pattern could be optimized and have a dedicated entry mode. This live programming REPL has a lot of nice advantages over a conventional REPL - i.e. one can immediately see how changes in code will change the outputs at each step.

*Aside:* Rendering for cells with simple types like `[1→x]` is obvious. However, Conal Elliott's work on [tangible values](http://conal.net/papers/Eros/) suggests that many functions may be usefully rendered. Developers can be given control by specifying a rendering context for common views of the spreadsheet, such that each cell `b3$foo` renders as `[b3$foo] render`. 

## IDE

There is no Integrated Development Environment (IDE) for AO at the moment, but I believe a specialized IDE could greatly mitigate AO's weaknesses:

* style and color should replace rendering of frequent prefixes or suffixes
* automatic word completion, fuzzy search sensitive to type and context
* hyperlinking or zooming to quickly access definitions and documentation
* automatic visualization of values or types, reducing mental burden
* graphical manipulation of rendered structures to simplify data shuffling
* automatic animation highlighting changes in structure across words in def
* automated refactoring support; discover similar code in other projects
* live programming; continuous feedback of value over change in definition
* development-time program search for data shuffling and other glue code

I envision such an IDE being developed as a wiki. AO's word-based module/function structure is very amenable to wiki based development. In the absence of such an IDE, AO has a steep learning curve and is not very human-friendly. I believe the quality of the AO programming experience will depend heavily upon available tools. 

### Flat Namespace

Each word in the AO dictionary has a global definition independent of local context. A single dictionary can grow to contain millions of words and support thousands of projects. This design offers several advantages:

* eliminates local import/export boiler plate 
* common language, learning, and refactoring across projects
* word is module, function, and software component
* great fit for wiki-based programming environment
* opportunity for discovery, reuse, knowledge sharing
* leads naturally towards dense namespace, terse code

Flat namespaces have one well known weakness: name collisions are easy on a global scale. Risk-averse developers will tend to use long disambiguating words, i.e. resulting in phrases such as `foo.projectQux bar.projectQux baz.projectQux` that grow verbose and almost intolerable to read or write. 

Fortunately, we can mitigate *or even reverse* this weakness in context of a rich development environment. 

An AO editor can recognize common prefixes or suffixes. Instead of rendering the full word, we could disambiguate using color or style. For example, `foo.projectQux` might render simply as `foo`, but in color blue. Similarly, on edit, auto-complete features with fuzzy find can simplify discovery and use of long words. Style configurations could be sensitive to the user and active project. 

This technique can feasibly *improve* readability, by enabling developers to see by color which words belong to which frameworks or libraries, and easily visualize how code is coupled. Thus, developers are encouraged to simply use a new suffix for words in a new project or framework. Widely useful code can be discovered and refactored later. 

## Multi-Stack Environment

AO words and literals operate on a tacit value. This value is structured as a `(stack * environment)` pair, which allows literals to be added to the stack while keeping the environment in a stable location (e.g. `42 :: (s*e)→((42*s)*e)`). The structure of the `environment` value is determined by convention. However, changing conventions can require widespread edits to the AO dictionary, so it also tends to be stable. In this sense, AO has a 'standard' environment that is implied by the current dictionary. Currently, AO has two standard environments:
 
        (stack*(hand*ext))
        (stack*(hand*(power*((stackName*namedStacks)*ext))))

* stack - the current stack where operations occur
* hand - auxillary stack to retain values, used as a semantic clipboard
* named stacks - collection of stacks to serve as workspaces, registers, etc.
* stack name - name of current stack, so we can switch workspaces
* power - a block containing `{tokens}` to invoke useful authorities
* ext - extensions slot, or unit value if not used
 
The first environment is common for 'pure' functions, e.g. invoked by the `apply` word. The second environment is a proper extension of the first, and includes multiple `(name*stack)` pairs to serve flexible roles: workspaces, registers, inventories, data storage. Multiple workspaces can be useful when modeling interaction (with implicit synchronization) between implicitly concurrent behaviors.

Long term, Awelon project is intended to take this concept much further - modeling a rich, personal user environment with flexible inventories and tools and navigation through workspaces. However, that form of environment is only suitable for live programming where users can have continuous visual reminders of structure. For an ahead-of-time, textual programming language such as AO, multiple named stacks for global state is near the human limit for complexity.

## Composition is First Principle

Composition is a primary principle of Awelon project. Composition means we can combine values using a small set of simple, uniform operators, with algebraic closure and other nice compositional (i.e. invariant or inductive) properties. Awelon bytecode and AO, however, only offer composition at the low level. To achieve composition at higher layers, developers must favor modeling problem domains and solution elements with value types that are themselves compositional. Some compositional types include:

* documents
* diagrams
* geometries
* relations/tables
* matrices
* grammars
* constraint models
* rulebooks
* scene-graphs
* streams
* widgets

In addition, we can have compositional mechanisms to surgically access and manipulate deep structure of a compositional model, such as [lenses](http://www.cis.upenn.edu/~bcpierce/papers/lenses-etapsslides.pdf) or [zippers](http://en.wikibooks.org/wiki/Haskell/Zippers).

There are many non-compositional models that are common in mainstream programming, such as records and ad-hoc objects. Even lists are at best weakly compositional (combining lists is too expensive). Developers can model these objects in AO, but it isn't recommended. Even if it initially seems a little awkward, moderately inefficient, or distorted in-the-small, finding ways to express problems and solutions compositionally is usually very rewarding in the long run and in-the-large.
