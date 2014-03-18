# Awelon Project

The Awelon Project is a realization of two relatively new models with the potential for disruptive improvement in human-computer interaction. 

**Awelon** is a personal user and development environment that can support lifelong operation. Structures in the environment represent documents, graphs, diagrams, geometries, even animated values - video, sound. User action in the environment - including navigation, copy-and-paste - are modeled by generating a stream of code to perform a transform on the environment. It is possible to extract and reuse or share subprograms from a user's action history. Thus, user macros or programming by example become widely feasible.

**Reactive Demand Programming (RDP)** is a reactive model designed for robust operation in open distributed systems. An RDP program is a behavior that continuously observes and influences external resources. RDP subprograms can represent services, frameworks, agents, and overlay networks. Essentially, RDP is a variation of spreadsheets for general purpose programming. See [AboutRDP](AboutRDP.md) for more. 

Awelon and RDP are related by a principle: objects in the user's environment have meaning to the user, and some of that meaning should be realized programmatically - by interpreting that structure as an RDP behavior. Consequently, manipulating a virtual structures can influence the real world. And virtual structure can also gather data for display.

## Components of Awelon Project

Awelon project requires a language that is suitable for streaming, code generation, code extraction, functional transforms and RDP. This requirement has led to development of Awelon Bytecode (ABC) and a higher level language for working with ABC, called Awelon Object (AO). See [AboutABC](AboutABC.md) and [AboutAO](AboutAO.md) for more.

Development of ABC and AO are necessary to an extent of bootstrapping and building a persistent web service with reasonable performance. Beyond that, an RDP implementation of ABC must be developed, and I would like to implement a wiki-based IDE for AO, and a spreadsheet-based interactive programming model. 

The personal Awelon user environment will happen later, most likely targeting augmented reality glasses. It is feasible to develop the Awelon user environment in the desktop space, and I plan to do so eventually. But in the short term, I believe people will be more open to a new user experience with glasses.

### Getting Started

At the moment, we have an AO compiler to ABC, a slow ABC interpreter, and a haskeline REPL. To get started, fork this github repo, clone your fork, then run 'cabal configure && cabal install' in the root directory. Ensure `~/.cabal/bin` is on your PATH, and add a new environment variable AO_PATH pointing to the cloned `ao` directory. 

The `aoi` executable doesn't allow you to define new words. You can edit the dictionary, however, and hit Ctrl+C to reload it at any time. The `ao` executable can run `test.` words (with `ao test`) or provide relatively imprecise type information (with `ao type word`).

Mind, AO is still in a very early stage. Its performance is only suitable for toy functions, e.g. loops of ten thousand steps. I still need to develop an optimizing ABC compiler. I'd like to shift AO development to the web, e.g. the wiki-based IDE mentioned earlier. Support for RDP, rather than just one-off functions and procedures, is in the future. Tools and interactive tutorials should make AO much more accessible, over the course of a year or two.

I'm willing to take contributions (pull requests) at this point, though they should come with a (non-exclusive) grant of copy rights. I'm not entirely sure how that works, but I don't want to juggle licenses. (Maybe I should switch to some kind of creative commons license, like Wikipedia uses? Suggestions welcome.)

## How can Awelon succeed where prior efforts failed?

Awelon is not the first effort to unify user interfaces with programming languages or integrated development environments.

However, Awelon project is perhaps the first to model the programmer as a first-class object in the language with no special privileges. A 'user model' of the programmer is essential! It enables the act of programming itself to be described and extended programmatically. Editor macros, views, paintbrushes, and IDEs become first-class functions. 

In addition, Awelon is unusually well suited to pattern recognition and automatic code generation. This is a consequence of ABC's and AO's concatenative nature and static typing. Program search can enable the sort of 'fuzzy' programming and refinement suitable for rapid prototyping and exploration. Pattern recognition and extraction of patterns from a user's history can support programming and control of systems by example. 

Technically, similar features are possible in other languages. But structurally complex languages have a much higher barrier for entry, a kind of [activation energy](http://en.wikipedia.org/wiki/Activation_energy) that hinders casual application.

Awelon also addresses a common point of failure. 

In many languages, objects become entangled with their environment. This can happen for a wide variety of reasons: library bindings, reflection, nominative types and versions, multimethods, deep closures, ambient authority and shared state. The consequence is that sharing fine-grained behavior has historically required 'shipping' the whole IDE. This hinders integration, reuse, and (importantly) *personalization*. We can hardly entrust private or personal information to an environment unless we can be very selective about what we share. Conversely, we cannot readily execute foreign code within our environment without a high degree of language security.

Awelon Bytecode is designed for secure code distribution, and it is not difficult to extract a function. Further, ABC's linear structure can be parsed and disassembled by an AO dictionary or other forms, thus users can read, inspect, tweak, and understand foreign code with their own words, independent of origin.

In short, Awelon can succeed because it offers less resistance against success.

