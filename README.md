# Awelon Project

The Awelon Project is a realization of two relatively new models with the potential for disruptive improvement in how we handle human-computer interaction. 

**Awelon** is a personal user and development environment, designed for lifelong operation. Structures in the environment represent documents, graphs, diagrams, geometries, even animated values - video, sound. These documents and objects can serve a role similar to widgets and applications. User action in the environment - including navigation, copy-and-paste - is modeled as acts of programming. Users can program-by-example across different resources, and can extract their histories into reusable tools.

**Reactive Demand Programming (RDP)** is a reactive model designed for live programming, persistence, and robust operation in open distributed systems. An RDP program is a behavior that continuously observes and influences external resources, capable of representing a service, framework, software agent, overlay network. Essentially, RDP is a variation of spreadsheets for general purpose programming. State in RDP systems is managed in external resources, simplifying orthogonal persistence and live programming. The notion of local, encapsulated state is expressed using linear types to exclusively bind state resources. See [AboutRDP](AboutRDP.md) for more. 

Awelon and RDP are related by a simple principle: structures in the user's environment have *meaning* to the user, and some of that meaning might be realized programmatically. That is, modifying the structure should have corresponding influence on the world. Or, conversely, the structure should gather and display information from the outside world - e.g. live documents or dashboards. This programmatic meaning is achieved by interpreting the user's environment as an RDP program. 

(For efficiency and zoomability, some behaviors are only active when the user is observing them, and various level-of-detail conditions may apply.)

Users are able to share values, documents, and any tools they create. 

## Augmented Reality

Awelon also has a goal of supporting augmented reality. Parts of the user's environment might be projected based on visual and GPS cues. In this case, the amount of metadata in the environment (regarding mapping to the real world) can often be greater than the useful data.

## Components of Awelon Project

Awelon project is realized by a suite of new technologies. Some of these will initially be validated in Agda, but the goal is to bootstrap and integrate these products into Android devices, Meta glasses, and web services. 

The current technologies under development are:

**Awelon Bytecode** (ABC) is a typesafe, tacit concatenative bytecode based on category theory and Hughes' arrows model. (ABC is tree-based, not stack-based.) ABC is designed primarily for RDP, but is also effective  for procedural or functional code. ABC serves multiple roles in Awelon project. It supports streaming, serialization, rewriting. ABC can record data structures and their history. ABC serves as a low-level distribution language that can be typechecked then compiled to LLVM or native code.

**Awelon Object Language** (AO) is a thin layer above ABC, providing modularity and word-level abstractions to make it more human-friendly. Like other tactic concatenative AO is effective for the full range of abstractions, low to high level. 

**Wiki-based IDE for AO** is an initial programming environment, which will operate in the browser.

After these technologies are developed and bootstrapped, development will start on the Awelon environment software.

Each user has a personal environment whose state includes documents, diagrams, graphs, and geometries. User action is implicitly modeled as a pure, streaming `state -> state` functions in a tacit concatenative (TC) language, operating on the environment. State is interpreted as a live RDP program, and stateful widgets with real-world effects can be modeled with simple program manipulations. State includes the full user model: navigation, focus, clipboard, lenses, tools. The user model is thus fully programmable. The TC stream history remains accessible for rewriting (including undo), review, and program extraction.

Awelon will include support for traditional KVM and web applications, but Awelon is ultimately aimed at augmented reality, augmented virtuality, and ubiquitous programming systems. 

Awelon is not the first effort to unify UI and IDE. But it is the first to formally model the user and treat language extensions (tools, lenses) as first-class objects. This change enables acts of programming to be implicit. RDP enables precise value sharing; there is no need to 'ship the IDE' to share behavior. And due to robust orthogonal persistence and capability-based security, users can keep important information in the environment without fear of losing or exposing it.



