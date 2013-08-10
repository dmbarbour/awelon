
Reactive Demand Programming Primer
==================================

Why RDP?
--------

RDP is a simple, reactive dataflow programming model, initially conceived (in 2010) as a means of leveraging functional reactive programming to extend and integrate open distributed systems. RDP has improved and matured in the years since. 

Reactive dataflow models are similar in nature to manipulating a spreadsheet: when values are changed, downstream computations will responsively adjust to accommodate the new values. However, traditional spreadsheets are insufficient for general purpose programming: they do not effectively address state, composition, extension,  distribution, dynamic behaviors, resource management, process control, time, and side-effects. General purpose reactive dataflow models, such as [SRP](http://en.wikipedia.org/wiki/Synchronous_reactive_programming) or [FRP](http://en.wikipedia.org/wiki/Functional_reactive_programming) (and now RDP) are similar with respect to the reactive dataflow aspect, all capable of representing highly responsive real-time applications. However, these different models diverge in those secondary aspects - state, time, effects, etc.. 

RDP more broadly and effectively addresses those secondary concerns compared to any of many other programming model (not limited to reactive models) that has reached my awareness. (And there are quite a few.) 

RDP has the following properties and corresponding features:

* **bidirectional** - reactive dataflow in RDP always goes both directions: observation AND influence. For example, consider observing a camera resource for its video stream. In addition to receiving an updating video frame over time, the camera will receive a signal indicating, at the very least, that someone is observing it. This is valuable for implicit resource acquisition and management: the camera may be powered down whenever there are no observers. Rich signals might carry more information, e.g. to influence pan, tilt, zoom, focus, and other camera properties.

* **duration coupling** - the output signal from every RDP behavior is active for the same contiguous durations as the input signal. This makes process control in RDP both trivial and precise: you can always halt a complex subprogram behavior by halting the input signal. This precise process control is, in turn, valuable for dynamic behaviors, live programming, upgrade, i.e. where we swap out one subprogram for another.

* **external state** - RDP pushes state into external resources, such as databases, registries, or filesystems. (*Note:* an RDP implementation may internally use memory for caching, memoization, and other purposes that are not *observably* stateful. But any formal state, even a simple accumulator, is managed externally.) This is a significant difference from other reactive dataflow models, e.g. FRP and SRP can use delay-feedback loops or integrals to model state internally. Forbidding local state is advantageous for dynamic behaviors, orthogonal persistence, and live programming - i.e. because we can swap out behaviors or halt the system without risk of losing encapsulated information. Further, external state is advantageous for *extension* in a manner similar to [publish-subscribe](http://en.wikipedia.org/wiki/Publish_subscribe) or [blackboard systems](http://en.wikipedia.org/wiki/Blackboard_system) - because we can uniformly add extra subprograms (or software agents) to observe and influence the external state without invasive modification.

* **commutative and idempotent effects** - the future state of a resource is strictly determined by its current state and the by the *set* of signal values influencing it. That is, duplicate signals, ordering within the set, history or future of specific signal sources, and so on must not observably affect resource state. This constraint on effects supports dynamic behaviors and staging (in terms of continuous deployment), supports rich equational reasoning and refactoring (very similar to pure functional programming), a plethora of optimizations (redundant code elimination, replication to distribute load, implicit content-delivery networks for flash crowds), enables eventual consistency as a safety net, and more.

* **spatial-temporal dataflow** - a single RDP behavior can reach out to many ad-hoc resources, gather information, and compute signals to influence yet other resources. RDP models these in terms of *latency* (e.g. for the time it takes to query a remote service) and *partitions* (i.e. so we can install RDP code near the resources). Partitions can also serve a role for heterogenous computation (e.g. CPU vs. GPU; server vs. client DOM), staged programming (e.g. modeling compile-time, link-time, or install-time environments, etc.).

RDP is an *extremely* expressive programming model, in the sense that a single RDP behavior can potentially serve many different roles: frameworks, overlay networks, software agents, applications, plugins, functions, queries, control or influence. RDP achieves this expressiveness without sacrificing performance: RDP supports implicitly batched updates and processing, logical synchronization, four layers to guard against observable inconsistency (latency, speculative evaluation, snapshot consistency, and eventual consistency), and many optimizations at multiple scales.

RDP is primarily suitable for programming of real-time control systems, distributed sensor networks, user interfaces, even interactive multi-user simulations (e.g. games). However, even for traditional 'batch' applications, RDP can offer advantages in terms of robustness, persistence, resource management, process control, live programming, and extensibility.


RDP Fundamentals
----------------

The fundamental concepts of RDP are **behaviors**, **signals**, **resources**, **partitions**, and **latency**. 

Behaviors are what RDP programmers define. A behavior describes a continuous operation that takes a signal as an input and generates a signal as output. As part of this process, signals may be sent continuously to external resources - to query or control them. Behaviors may be impure, but even impure behaviors are not stateful (i.e. you never lose information by replacing a behavior with a fresh copy of itself). The concept of 'pure' signals has some value for security and safety reasons.

Resources are external to RDP. Developers can only influence or observe a resource from afar. Anything physical (and programmable) is necessarily a resource - monitor, mouse, keyboard, speakers, robotic arms, motors, and so on. In addition, a system may provide 'virtual' resources, such as files, databases, window areas. The resources available to a developer may depend upon an AVM, or upon searching a shared registry, or upon first-class dynamic behaviors (i.e. object capabilities).

Signals are hidden within RDP. Developers never directly create or manipulate signals. Nonetheless, the conept of signals is useful when understanding behavior types or how behaviors interact with resources. Typically, a signal represents the actual or desired state for a resource, such as the position of a mouse or the contents to be displayed in a window. Signals may be *atomic* or *complex*. An atomic signal is like a time-varying integer: the integer is the smallest unit we're watching for changes, and it exists at a specific location in space-time (partition and latency). A complex signal may model concurrency (`x & y`; signals that are logically active at the same time, like a pair) or switching (`x + y`; activity switches between signals based on some upstream decision, like a union). Complex signals may be distributed in space and time.

Partitions represent *programmable locations*, often abstractly in terms of generic 'kind' - desktop, phone, cloud, client, server, CPU, GPU. Different partitions typically have access to different resources, and potentially even differ in what pure computations and types are feasible (e.g. GPU is much less flexible than CPU). An RDP application will usually start in one partition (where a 'go' signal is provided), but signals can be sent to different partitions using a "cross" behavior (like crossing a bridge), and code may further specify what should happen once the signal is received at its destination. (By design, RDP can always send the code with the initial signal.)

Partitions can also serve a few other purposes, e.g. conceptual locations like Void (from which no signal escapes), Static (for compile-time computations), or staged computing (modeling link-time or install-time as partitions). 

Latency is the 'time' concept, and is only meaningful within context of a behavior. Latency is measured relative to the start of a behavior. Latency serves a few roles. Most significantly, atomic signals cannot be combined (e.g. to add two integer signals) unless they have the same latency. (That property is essential to protect *duration coupling*.) Additionally, some signal values may be typed with use-before and use-no-earlier-than latency values, to model temporal constraints. 


RDP Patterns and Idioms
-----------------------

RDP plus even a simplistic state resource forms a Turing complete system. Any solvable problem can be solved using RDP. However, some solutions will be more or less awkward than others. Developers are encouraged to think of problems in new ways to solve them within the constraints of RDP.


