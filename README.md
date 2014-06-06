# Awelon Project

The Awelon Project is a realization of two relatively new models with the (long term) potential for disruptive improvement in human-computer interaction. 

**Awelon** will be a personal user and development environment that can support lifelong operation. Structures in the environment represent documents, graphs, diagrams, geometries, even animated values - video, sound. 

User action in the environment - including navigation, copy-and-paste, sliders, text editing, etc. - are modeled as generating a stream of bytecodes to perform a transform on the user's environment. By keeping a user history, we can extract from this stream of bytecodes to create macros and tools, generalizing programs from a few examples. The quality and robustness of programs we can extract depends heavily on the bytecode, so Awelon project includes a new 'Awelon Bytecode' (ABC). 

**Reactive Demand Programming (RDP)** is a reactive model designed for robust operation in open distributed systems. Every RDP program is a behavior that continuously observes and influences external resources. RDP subprograms can represent services, frameworks, agents, and overlay networks. Essentially, RDP is a variation of spreadsheets for general purpose programming. See [AboutRDP](AboutRDP.md) for more. 

For Awelon Project, RDP is important because it is amenable to 'live programming' on a large scale. Users manipulate the program and get feedback in real-time. By treating a user's personal environment as a live program, actions on that environment become formally meaningful and effectful. Manipulating a geometry - moving a virtual slider or flipping a virtual switch - can influence lights and sounds in the real world. 

The macros and tools extracted from user actions can be directly leveraged to interpret the environment.

## Components of Awelon Project

Awelon project requires a language that is suitable for streaming, code generation, code extraction, functional transforms and RDP. This requirement has led to development of Awelon Bytecode (ABC) and a higher level language for working with ABC, called Awelon Object (AO). See [AboutABC](AboutABC.md) and [AboutAO](AboutAO.md) for more. Regular users of Awelon are be expected to manipulate AO or ABC directly, but they will be accessible for programmers and power users.

Development of ABC and AO are necessary to an extent of bootstrapping and building a persistent web service with reasonable performance. Beyond that, an RDP implementation of ABC must be developed, and I would like to implement a wiki-based IDE for AO, and a spreadsheet-based interactive programming model. 

The personal Awelon user environment will happen later, most likely targeting augmented reality glasses. It is feasible to develop the Awelon user environment in the desktop space, and I plan to do so eventually. But in the short term, I believe people will be more open to a new user experience with glasses.

### Status

Libraries and highly performant implementations are still lacking. ABC potentially permits many optimizations, but I've been aiming to delay work on those until I can implement them from within AO. 

For now, we do have a REPL interpreter `aoi` and a command line multi-utility `ao`. Performance is mediocre, suitable for small toy programs but not for full services and applications. Current efforts are on improving dynamic compilation (via Haskell's System.Plugins) such that we can achieve excellent performance in real service/application scenarios.

### Getting Started

At the moment, we have an AO compiler to ABC, a slow ABC interpreter, and a haskeline REPL. To get started, fork this github repo, clone your fork, then run 'cabal configure && cabal install' in the root directory. Ensure `~/.cabal/bin` is on your PATH, and add a new environment variable AO_PATH pointing to the cloned `ao` directory. 

The `aoi` executable doesn't allow you to define new words. You can edit the dictionary, however, and hit Ctrl+C to reload it at any time. The `ao` executable can run `test.` words (with `ao test`) or provide relatively imprecise type information (with `ao type word`).

Mind, AO is still in a very early stage. Its performance is only suitable for toy functions, e.g. loops of ten thousand steps. I still need to develop an optimizing ABC compiler. I'd like to shift AO development to the web, e.g. the wiki-based IDE mentioned earlier. Support for RDP, rather than just one-off functions and procedures, is in the future. Tools and interactive tutorials should make AO much more accessible, over the course of a year or two.

I'm willing to take contributions (pull requests) at this point, though they should come with a (non-exclusive) grant of copy rights. I'm not entirely sure how that works, but I don't want to juggle licenses. (Maybe I should switch to some kind of creative commons license, like Wikipedia uses? Suggestions welcome.)

## How can Awelon succeed where prior efforts failed?

Awelon is not the first effort to unify user interfaces with programming languages or integrated development environments.

However, Awelon project is perhaps the first to model the programmer as a first-class structure in the language with no special privileges. I believe this is essential: the 'programmer model' acts as the bridge between use of a structure (user interface) and interpretation or translation of a structure (programming). With the first class programmer model, we can extend the user interface for programming and extract reusable programs from normal use.

In addition, Awelon is unusually well suited to pattern recognition and automatic code generation. This is a consequence of ABC's and AO's concatenative nature and type safety. Program search can enable the sort of 'fuzzy' programming and refinement suitable for rapid prototyping and exploration. Extraction of patterns from a user's history can support programming by example. 

Similar features are technically possible in other languages. But structurally complex languages have a much higher barrier for entry, a kind of [activation energy](http://en.wikipedia.org/wiki/Activation_energy) that hinders casual application. The entanglements created by namespaces, nominative types, library versioning, closures, reflection, aliasing, ambient authority... create a very challenging environment for the sort of precise tool sharing and personalization users need. 

If Awelon succeeds, it's because Awelon offers less resistance against success.
