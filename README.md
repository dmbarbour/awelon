# Awelon Project

The Awelon Project is a realization of two relatively new models:

**Reactive Demand Programming (RDP)** is a reactive model designed for open distributed systems. An RDP program is a behavior that continuously observes and influences external resources, capable of representing a service, framework, software agent, overlay network. Essentially, RDP is a variation of spreadsheets for general purpose programming. State in RDP systems is managed in external resources, simplifying orthogonal persistence and live programming. The notion of local, encapsulated state is expressed using linear types to exclusively bind state resources. See [AboutRDP](AboutRDP.md) for more. 

**Awelon Environment (Awelon)** is a unified user interface and integrated development environment. Each user has a personal environment whose state includes documents, diagrams, graphs, and geometries. User action is implicitly modeled as a pure, streaming `state -> state` functions in a tacit concatenative (TC) language, operating on the environment. State is interpreted as a live RDP program, and stateful widgets with real-world effects can be modeled with simple program manipulations. State includes the full user model: navigation, focus, clipboard, lenses, tools. The user model is thus fully programmable. The TC stream history remains accessible for rewriting (including undo), review, and program extraction.

Awelon will include support for traditional KVM and web applications, but Awelon is ultimately aimed at augmented reality, augmented virtuality, and ubiquitous programming systems. 

Awelon is not the first effort to unify UI and IDE. But it is the first to formally model the user and treat language extensions (tools, lenses) as first-class objects. This change enables acts of programming to be implicit. RDP enables precise value sharing; there is no need to 'ship the IDE' to share behavior. And due to robust orthogonal persistence and capability-based security, users can keep important information in the environment without fear of losing or exposing it.

Awelon is a true user environment, intended for lifelong operation, not merely an IDE.

The Awelon project is a consists of several parts:

* **Awelon Bytecode** (ABC) - a minimal, tacit concatenative, typed, capability-secure streamable bytecode for heterogeneous open and distributed systems. 
* **Awelon Language** - a trivia

# Awelon

Awelon is a richly typed, compiled, tacit programming language for reactive demand programming (RDP). RDP is a general purpose, reactive dataflow model that addresses challenges involving open, distributed, and heterogeneous systems. In Awelon, a single application can include code that executes across a distributed network. 

This Haskell project will provide a compiler for Awelon.

See AboutAwelon or AboutRDP for more. 


