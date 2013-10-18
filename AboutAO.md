# Awelon Object Language (AO)

Awelon Object language (AO) is a thin layer above Awelon Bytecode (ABC) that introduces the concepts of words and definitions, and is much more human friendly. AO achieves a degree of modularity: words can in many cases be developed and maintained separately from the body of code that uses them. 

An AO definition is a sequence of words, literals, and inlined ABC.

Each word has one definition. The meaning of a word in a sequence is simply the static, inline expansion of its definition. The semantics of an AO program is simply its expansion into ABC. NOTE: It is an error for AO definitions to be cyclic. Loops in AO must be represented using fixpoint combinators, as they are in ABC. 

ASIDE: ABC's lack of modularity, dictionaries, and definitions is a deliberate design choice. Where we have definitions, we must also manage the scopes, versions, lookup. And we must concern ourselves with cyclic definitions. These properties are difficult to manage at a low level, or in a networked system. Also, definitions interfere with decomposition and streaming.

## Numbers, Text, and Blocks

For numbers, AO supports a few representations:

        42         (integral)
        -12.3      (decimal)
        2.1e-3     (scientific; eN means *10^N)
        34.5%      (percentile; same as e-2)
        1/3        (rational)
        0xbad1dea  (hexadecimal)

All representations are interpreted as exact rational numbers. (Like ABC, AO can only access floating points via capabilities.) Note that spaces aren't allowed within a number,

AO supports both block text and inline text. In both cases, text is prefixed with a double-quote. But block text must start at the beginning of a new line, and inline text must start after a space.

        "Block text starts at newline with double quote
         then continues to the end of line unless
         the next line has an indent space, in which case
         the text continues, after inserting a newline.
         The indent space is not part of the text.

        ... "inline text cannot start a newline" ...

Inline text is useful mostly for short symbols like `"foo"` or similar, where breaking it into a newline would break the flow. AO has no concept of escape characters in text. If arbitrary text is required then a block format should be used. (Or use text manipulations.)

Blocks in AO are very similar to blocks in ABC. The only difference is that blocks contain AO code instead of ABC code:

        [swap assocl swap assocl swap]

The square bracket characters also terminate words.

### Literals and the Environment

In any tacit concatenative language, a literal is a pure function that inserts a static value into the environment. However, AO and ABC insert literals at slightly different locations. For ABC, literals have type `x -> L*x` (where L is the type of the literal). For AO, literals have type `(x*e) -> ((L*x)*e)`. (Effectively, there is an implicit `%l` inserted just after each AO literal.)

The reason for this difference is to ensure that part of the AO environment (`e`) has a stable relative location even when introducing literals. In AO convention, the `x` position typically represents the current stack, while `e` represents the user model and global environment.

## Inlined ABC

AO enables developers to inline a useful subset of ABC. 

        %slsls

Inlined ABC is a pseudo-word prefixed with `%` then followed by a sequence of ABC, and terminated by whitespace. The limitation on inlined ABC is that it cannot contain literals (text, blocks, numbers or `#`) or whitespace. Literals must be encoded in the AO layer, i.e. `[%slsls]`. Whitespace in ABC just means identity, so isn't a loss.

AO does not have primitive or built-in words. Inlined ABC serves that role. 

## Anatomy of a Word

AO does not have any syntax for defining a word. The relationship between a word and its definition is maintained externally to AO, i.e. as part of the programming environment, or potentially in another layer of language. Consequently, AO code is just code - there are no definitions, no declarations.

Words in AO text must be trivially distinguishable from numbers, text, blocks, and inlined ABC. Further, words in text are separated by whitespace, and AO code should generally be visible to a human user or when printed. I currently favor the following as a minimal set of restrictions:

* words cannot use control characters (C0, C1)
* words cannot use space (32), delete (127)
* words cannot contain `[`, `]`, or `"`
* words cannot start with a digit (0-9)
* words cannot start with `%`

A programming environment may impose a few more syntactic restrictions on words, e.g. to eliminate Unicode characters that are ambiguous when printed. When code is obtained from another environment, there should be some support for globally renaming invalid words. But, in general, words may contain punctuation, can appear as `{foo` and `/foo}` brackets, etc.

## Flat Namespace

AO has a flat namespace. There are several benefits of favoring a flat namespace. 

1. users learn words, not projects
2. no boilerplate import/export management
3. easier integration and communication across projects
4. useful collisions: discovery, reuse, knowledge sharing
5. resolve conflict, don't avoid it: dense namespace, terse code

Deep, hierarchical namespaces can avoid collisions and conflicts. However, the overhead of this avoidance is significant: import/export boiler plate, and difficulty discovering or reusing functions buried deep in other projects. Further, the benefits of avoiding collisions are marginal, since they are not inherently difficult to resolve. Any refactoring tool can do the job.

## AO Development Environments

AO is unsuitable for the legacy text editor and files programming environment. AO certainly could be represented that way, but files are simply a poor fit for organizing a very large number of very small definitions.

This project will implement two development environments for AO. 

1. a wiki-based IDE as a web service, accessible by normal web browsers
2. an AO development environment implemented fully within AO

A wiki-based programming environment might manage thousands of projects and users within a common dictionary. I wish to test a hypothesis: that cross-project refactoring can lead to much greater reuse, and that some combination of code search, pattern recognition, and namespace collisions can become a collective spark for cross-project refactoring. There may be DVCS-like mechanisms to fork, push, pull, merge at the whole-wiki level. Thus, a company or individual could fork the wiki, and push useful code back upstream when they wish to share.

The AO development environment would be more personal in nature, as the AO environment is intended to be a personal user environment. 

In addition to managing words and definitions, a good AO environment should provide many more features:

* keep multiple definitions on-screen
* render the environment, static or runtime
* animate the environment across words, play in a loop
* live update of code
* orthogonal persistence
* extension of the environment (new features, user macros)
* import and export of words
* extract ABC code from a word
* disassemble ABC code to current dictionary
* optimize ABC code
* compile ABC code to native executables
* documentation, or literate programming
* version control and good view of history
* active / continuous testing
* active application platform

Note that AO has no effective built-in notion of comments. Presumably, we could represent comments in terms of writing some text then dropping it. But I think that would not be pleasant to write or read. For AO, the model for documenting code is left to the environment. AO omits a syntax for comments to avoid competing modes of documentation. I am interested in exploring interactive documentation, and potentially extracting the definition of a word from some larger documentation-of-the-word model.

While text files are not the preferred mechanism for editing AO, files are still useful as an import/export medium. An interesting approach is for export files to contain ABC code that builds a dictionary - and potentially records some history.

A more interesting possibility is an IDE that provides a wiki-like editing environment. Words are stored behind the scenes in ACID table, 



 for use in a wi

 This is left to the programming environment, and de-facto standardization.

However, the following options are suggested:


## The Anatomy of a Word

The syntax of a word is very flexible. The main restriction is that a word must be trivially distinguished from numbers, blocks, text, inlined ABC, and other words. By 'trivially', I mean that a lexer shouldn't require any lookahead. 

A secondary restriction is that it shouldn't be difficult to print AO code. So invisible or control characters should be avoided. 

In concrete terms, I suggest the following restrictions:

* a word cannot start with a digit (0-9)
* a word cannot start with `[` or end with `]`
* a word cannot start with `"`
* a word cannot start with `%`
* a word cannot contain C0, C1, space, delete



## Envisioned IDE Support 

A good IDE for AO should at least have the following features:

* The expected type of the environment should always be rendered.
* Tab completion: when a developer begins writing a word, all words with a similar start should be provided.
* 
* A set of potential words can be 
* A markov model should provide predictions about which words a developer is likely to write next.
* It should be easy to create REPL-like tests that are continuously evaluated as definitions are edited. 

AO is suitable for use in a wiki-like programming environment where potentially hundreds of projects are developed and maintained. A social benefit of a flat namespace is that it becomes much easier to speak of words with a standard meaning across projects. Effective refactoring and reuse of words between projects can also greatly reduce the overhead of keeping many projects together. 

If words are too highly contextual, it can be difficult to communicate or specify them in a social context.


 While this development is mostly independent, collisions provide a point for collaboration and cross-project refactoring. 




If a single word is used with two different meanings, then the collision can be resolved by a simple name refactoring. 

## Words

Syntactically, words in AO are quite flexible. The main limitations:

* cannot start with digit (0-9)

Essentially, a word shouldn't contain characters that would make it non-trivial to distinguish from numbers, blocks, text, or inlined ABC, or difficult to print. Also, spaces and newlines are used to separate words when AO is encoded in UTF-8 text. (Developers should use Unicode non-breaking spaces if they, for aesthetic reasons, desire spaces in their words.) 

The structure of a word has no formal meaning. However, words may have some conventional structure for presentation purposes, or to support lookup. In this case, it may be that an IDE typically only renders part of the structure, i.e. such that instead of `draw` we might have `draw{art}` vs. `draw{curtains}`, but only render 'draw' by default. 

Potentially, we could render different words in different colors (

 






cannot start with a digit, cannot start with `[` nor end with `]`, and cannot start with `"` or `%`.

Words are not interpreted for meaning.






Potentially, a wiki-based programming environment might render only part of a word E.g. if a word is of the form `draw|arts` then perhaps the system only renders 'draw' unless there is some local colls, enabling the same word to be used but actually link to different definitions. 

If we wish to maintain the original 'view' of the word, then potentially we need an IDE that has a few conventions for how to render certain words. E.g. perhaps `draw{art}` is a different word than `draw{gun}`, and link to different full definitions, but both are rendered just 'draw' based on some naming convention. (Similarly, there may be support for colored words.)



where it is easy to navigate one definition to another. 

Name collisions are a good thing when they cause developers of different projects (with some overlap in problem domain) to discover one another. This becomes an opportunity for collaboration, reuse, or sharing of knowledge. Collisions can be resolved easily

AO is designed for a wiki-like development environment. This might be a centralized wiki that integrates code from hundreds of projects, 

Conflict can be a good thing when it leads to developers of different projects with overlappin discovering one another. 


AO is intended for programming in wiki-like or DVCS-based environments. A single development wiki might host hundreds or thousands of projects, and naturally some devewill wish to use similar words 
 or DVCS environments that can pull from multiple sources. 

AO does not manage namespaces. There is no boiler-plate list of imports or exports. There are no declarations or headers. 


AO's philosophy favors a different approach







Naturally, this can lead to some conflicts when integrating code from different projects. 


If there is any ambiguity in the namespace, it must be eliminated externally.



AO is intended for wiki-like or DVCS based programming environments, where code contains all the definitions it needs internally even if they occaisonally update from external sources.


AO is intended for a wiki-like programming environment, where 

That is, there is no support for "using" directives or similar. 

## Background


Similarly, the decision toward a flat namespace - words and definitions - is also deliberate.


To keep code pretty




This decision eliminates the import/export boilerplate within each module. This design is suitable for a wiki-like development environment, and is intended for a DVCS-like system where each project keeps its own set of definitions - but tend to share through common repositories. Name conflicts in a shallow namespace are inevitable, but will be resolved by social mechanisms. My hypothesis/hope is that this will lead to better communication and code reuse between developers of different projects.

Of course, there will also be mechanisms to systematically rename words from a particular source, when loading a large set of modules into the environment.

## Flat Namespace




## Inlined ABC


## Literals


## Flat Namespace

AO has a 'flat' namespace. There is no support for shorthand, though an IDE might provide support for names rich with 


An AO module relates one word with one definition. The 'modularity' in this system is due to separate maintenance of words. It is also possible that some words examine their context. 

An AO definition is a sequence of words and literals. The meaning of a word used in AO is the inline expansion of its definition. Literals include numbers, text, blocks of code, and inlined ABC. 

It is an error for AO definitions to be cyclic. It is also an error for a word to be ambiguous.



## Representation





The meaning of a word is just the inline expansion of its definition. Literals include numbers, text, blocks, and inlined ABC. The full expansion of an AO program is ultimately the sequence of literals and inlined ABC. 





The 

Cyclic and ambiguous definitions are an *error* in ABC. 


An undefined word 



In a filesystem, this might be represented as an **.ao** file, where the filename is the module name and the file contents is the definition. However, it is also possible to have dictionary files that carry many definitions. A definition is a sequence of whitespace separated words and literals. 




A word is any sequence of non-whitespace characters not starting with a number. Note that punctuation can be a word.



(Whitespace is not significant.)



NOTE: ABC code is perfectly capable of modeling an environment with definitions. It is not difficult to do, e.g. modeling lookups in an association list. However, this approach to lookups is not very modular.

Definitions introduce issues of scoping and lookups. Modules introduce issues of independent 

were separated from ABC because they make streaming much more difficult. 



AO code looks like a sequence of words, each word separated by whitespace. In a filesystem, the **.ao** extension should be used for each file, with the filename corresponding to the word and the file cont simply contains its definition.

 This greatly simplifies parsing and processing of AO code. 

AO code is free to invoke ABC code directly (which in turn may invoke any c



By 'definitions' I mean that ABC code operates at the level of words. Each word is separated by whitespace. 

By 'modularity' 

Modularity was eschewed from ABC because it interferes with streaming, requires too much implicit memory. 

 language is a very thin layer above ABC code. 

AO only introduces two features above ABC: word level definitions , and modularity (the ability to independently maintain definitions).




Primarily, AO can be easily extended with new words

The main difference is 
