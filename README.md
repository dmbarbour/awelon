# Awelon Project

The Awelon Project is a realization of two relatively new models with the potential for disruptive improvement in human-computer interaction. 

**Awelon** is a personal user and development environment that can support lifelong operation. Structures in the environment represent documents, graphs, diagrams, geometries, even animated values - video, sound. User action in the environment - including navigation, copy-and-paste - are modeled by generating a stream of code to perform a transform on the environment. It is possible to extract and reuse or share subprograms from a user's action history. Thus, user macros or programming by example become widely feasible.

**Reactive Demand Programming (RDP)** is a reactive model designed for robust operation in open distributed systems. An RDP program is a behavior that continuously observes and influences external resources. RDP subprograms can represent services, frameworks, agents, and overlay networks. Essentially, RDP is a variation of spreadsheets for general purpose programming. See [AboutRDP](AboutRDP.md) for more. 

Awelon and RDP are related by a principle: objects in the user's environment have meaning to the user, and some of that meaning should be realized programmatically - by interpreting that structure as an RDP behavior. Consequently, manipulating a virtual structures can influence the real world. And virtual structure can also gather data for display.

## Components of Awelon Project

Awelon project requires a language that is suitable for streaming, code generation, code extraction, functional transforms and RDP. This requirement has led to development of Awelon Bytecode (ABC) and a higher level language for working with ABC, called Awelon Object (AO). See [AboutABC](AboutABC.md) and [AboutAO](AboutAO.md) for more.

Development of ABC and AO are necessary to an extent of bootstrapping and building a persistent web service with reasonable performance. Beyond that, an RDP implementation of ABC must be developed, and I would like to implement a wiki-based IDE for AO.

The personal Awelon user environment will happen later, most likely targeting Meta glasses. It is feasible to develop the Awelon user environment in the desktop space, and I plan to do so eventually. But in the short term, I believe people will be more open to a new user experience with glasses.

## How is Awelon Different?

Awelon is not the first effort to unify user interfaces with programming languages or integrated development environments.

However, Awelon is perhaps the first to model the programmer as a first-class object in the language with no special privileges (other than 'undo'). This change is essential: it enables much programming to be far more implicit, in the form of macros, tools, and lenses modeled within the language and extracted from user history. 

Awelon also addresses a common point of failure. In many languages, objects become entangled with their environment. This can happen for a wide variety of reasons - library bindings, reflection, references to shared state, nominative types. The consequence is that sharing fine-grained behavior often requires shipping the whole IDE. This hinders integration, reuse, and (importantly) *personalization*. One can hardly entrust personal information to an environment when sharing behavior is coarse grained.

Awelon Bytecode is designed to simplify precise extraction and sharing of code. 



