
# Reactive Demand Programming

Reactive Demand Programming, or RDP, is a reactive dataflow programming model designed for orchestrating open systems. RDP is suitable for modeling overlay networks, multi-agent systems, shared services, and first-class frameworks. RDP differs from other reactive models - such as [synchronous reactive programming](http://en.wikipedia.org/wiki/Synchronous_reactive_programming) and [functional reactive programming](http://en.wikipedia.org/wiki/Functional_reactive_programming) - in how RDP addresses the concerns of **side-effects**, **state**, **resource management**, **distribution and scale**, **extension and open systems**, and **live update or upgrade**.

In practice, RDP should be augmented with [linear types](http://en.wikipedia.org/wiki/Substructural_type_system). Doing so addresses RDP's few weaknesses. 

At runtime, RDP is push-based. Fine-grained signal updates are aggregated into batches that may be sent to a remote system (or even a different thread). RDP updates can be modeled over a number of protocols, including HTTP, long polling, WebSockets, etc.. Updates are generally speculative, reporting expected future values and correcting them as needed. Fine-grained (per signal) speculation and retroactive correction enable high levels optimistic parallelism and some very nice consistency properties (cf. [time warp protocol](https://www.google.com/search?q=time+warp+protocol)). 

RDP is designed for code distribution in mutually distrustful systems. I.e. some service interfaces should allow for signals containing code. *Awelon* is intended to be the primary distribution language. RDP and Awelon both address various security concerns that would otherwise hinder code distribution (see below).

## Side Effects in RDP

RDP has side-effects without giving up rich reasoning and refactoring properties. This is possible because RDP constrains side-effects very severely. Despite these constraints, RDP is capable of expressing any application.

Side-effects in RDP are expressed as **signals** being sent to **resources**. A resource is something logically external to RDP - e.g. video display, joystick, or database. At any given time, the state of a resource may be influenced by a *set* of concurrent signals. Observers of a resource may see parts of its state, represented as a signal. Resources are accessed and observed from RDP by use of **behaviors**; an RDP application is just a composition of behaviors that observe some resources, make decisions, and influence other resources. 

In RDP, side-effects are:

* **idempotent and commutative** - a resource's future state is influenced by its current state and the set of signal values reaching it at the logical instant. The word 'set' here is strict: duplicate values and ordering within the set must not influence resource state in an observable way. This constraint supports rich equational reasoning and refactoring, transparent mirroring and failovers, code-sharing and content-distribution optimizations, and transparent switching of dynamic behaviors. 
* **continuous** - signals in RDP are piecewise-continuous; they do not have "instantaneous" values. This can make it awkward to directly express some operations - i.e. you cannot demand `x++` because it does not make sense to do so continuously. Fortunately, there are effective idioms for addressing such problems.
* **controlled** - RDP enforces an invariant called *duration coupling*: the output signal of a behavior has the same active duration as the input signal. E.g. if you observe mouse position, you only receive the mouse signal for *as long as* you're observing it. This property makes the majority of resource control very easy in RDP: resources are acquired when there is at least one observer or influence, and released after none remain. This property also makes process control very easy in RDP: you can halt any program or subprogram by stopping the signal to it. 
* **concurrent and open to extension** - resources, including state, are external to RDP. In general, a resource may be influenced by multiple signals from different RDP subprograms. RDP strongly encourages 'collaborative' resources that can tolerate influence from multiple agents, such as tuple spaces, publish-subscribe, and concurrent-constraint models. Such resources result in very extensible, pluggable, systems. (*Note:* linear types can be used with RDP to express exclusive control.)
* **potentially speculative or retroactive** - Many resources can support speculation or prediction of future states, especially if they have the speculative future input signals. Similarly, many resources can tolerate a small window for retroactive correction to input signals. Between them, prediction and correction offer robust consistency properties across minor 'hiccups' in network or scheduling latencies.

Idempotence, commutativity, continuity, and concurrency give RDP a very declarative *feel*. Subprograms in RDP, especially those on separate pipelines, can be understood as declarations about how parts of the should be connected.

*Trivia:* The word 'demand' in 'reactive demand programming' refers to the set of signals influencing a resource. The common case is that, resources are "demand driven". They enter some kind of hibernation mode when there is no active demand. Further, these signals can carry values, e.g. representing authoritative queries or requests - demands. Compared to 'command', the word 'demand' also has nice connotations of being declarative and continuous.

## RDP Basics: Signals, Resources, and Behaviors

There are three central concepts in RDP: signals, resources, and behaviors. A reactive demand program is directly concerned with the description of a behavior. Behaviors interact with resources through signals, so an understanding of all three concepts is essential.

### Signals

A **signal** in RDP is a time-varying value with a location and lifespan. A signal has a definite start time and an indefinite end time. A signal will often represent continuous queries or control, or the response to a query. For example, we might query the mouse position with a trivial unit signal, and the response should be a signal describing the time-varying position of the mouse. 

RDP signals operate in a real world timeline. However, this time is tracked logically; for example, we can model that a signal update occurs at *exactly* noon, without concern for whether the update arrives at exactly noon. (Even better it arrive a little before noon.) There is also no risk of 'missing' intermediate values for a signal, even short-lived values like button-down during a mouse click.

Signals are often *composites*, i.e. bundles of simpler signals. 

There are two basic composite signals: `(x * y)` products and `(x + y)` sums, where x and y may also be composites. Different signals in a composite can update at different rates and times - i.e. they enable an asynchronous implementation. Products model concurrent operations; both signals are available for the same durations. Sums model switching networks or choice; the durations of x and y are additive (if x is active for an hour, then y must be inactive for an hour).

Collection-oriented composites are also possible, i.e. a vector, matrix, or set of signals. Collection-oriented RDP could be useful for a variety of problem domains.

#### Spatial-Temporal Signals: Partition and Latency

To support distribution, signals in RDP have location - i.e. there is a difference between a time-varying integer on the server vs. a time-varying integer on the client. Or between CPU and GPU. Or even between one thread and another. This is modeled in terms of **partitions**, which distinguish locations physical or logical. Communication between partitions is usually explicit, enforced by the type system.

In addition to partition, runtime signals have **latency** properties. It can take time to communicate a signal between partitions, or to compute values, and this is described by logical static latency. The primary reason to track latency is to enforce RDP's duration coupling invariant: two time-varying integer cannot be added unless they have the same partition *and* latency. Introducing latency (via `delay` behavior) is also a useful technique to address consistency problems caused by straggling updates, and to control feedback loops when interacting with resources.

Due to variation in logical latency there may be times where active durations for `(x + y)` overlap, and times where durations for `(x * y)` do not overlap. In this sense, composites are logically asynchronous. Signals must be explicitly synchronized for some operations, such as adding numbers.

Though, asynchrony with static, logical latencies is qualitatively different than what most programmers consider asynchronous. I think the "update at different rates and times" feature for composite signals is a closer fit to what is popularly considered asynchronous.



### Resources


A **resource** is external to RDP yet open to programmatic observation or influence. Resources broadly include sensors, actuators, and state. Specific resources might be mouse, keyboard, video displays, speakers, robotic arms, motors, physical storage, and virtual resources such as files, canvases, or subwindows. An RDP system must provide some behaviors, as primitives or capabilities, to access external resources. The biggest challenge in making RDP useful is integrating a wide variety of resources. 

Fortunately, RDP signals are a good fit for most sensors and actuators, and it is not difficult to invent RDP-compatible state models. Unfortunately, there are too many sensors and actuators to integrate directly, and adapting existing state resources can be awkward. The approach under consideration is to first support web application services for UI, then to integrate a pubsub model (likely [ROS](http://www.ros.org/wiki/) or [DDS](http://portals.omg.org/dds/)) to help integrate the broad variety of physical devices. After that, specific resources can be integrated with RDP to improve quality of service (temporal precision, latency, speculation, etc.).





, or [CCN](https://www.ccnx.org/wiki/CCNx/CCNxProtocolDescription)) to help, at least in the short term. The quality of resource control - precise timing, speculation, etc. - would likely suffer in this design. But it should make RDP more immediately useful, and the critical resources can then be adapted at their own pace.

This would reduce the burden on compiler development.

though the quality of integration may suffer a little.

 services).

State in RDP always belongs to a resource. 






### Distribution, Parallelism, Heterogeneous Effects

Not all the resources used by an RDP application need to be on one physical box; RDP is designed for distributed applications. Sign


Due to use of logical time, networked RDP requires clocks to be synchronized. An NTP configuration should be sufficient. 

but is robust to small drift. An NTP configuration should be sufficient to keep clocks synchronized for RDP, and barring that an RDP implementation can maintain relative offsets 


Resources in RDP may also be *distributed*

* **spatial-temporal dataflow** - a single RDP behavior can reach out to many ad-hoc resources, gather information, and compute signals to influence yet other resources. RDP models these in terms of *latency* (e.g. for the time it takes to query a remote service) and *partitions* (i.e. so we can install RDP code near the resources). Partitions can also serve a role for heterogenous computation (e.g. CPU vs. GPU; server vs. client DOM), staged programming (e.g. modeling compile-time, link-time, or install-time environments, etc.).



## State Resources and Linear Types

## Code Distribution in Mutually Distrustful Systems

Code distribution is common and useful, e.g. for flexible web applications, multi-agent systems, disruption tolerance, latency control, or overlay networks. But trust is often asymmetric: e.g. clients must trust servers but not vice versa. To make code distribution practical, usable, and widely acceptable, many concerns must be addressed:

1. we must precisely and explicitly control which authorities are granted
2. there should be no implicit grant of authority just for running code locally; it should be a performance and safety concern only (bandwidth, latency, disruption tolerance, etc.).
3. we must always be able to revoke granted authorities (no [grandfather clause](http://en.wikipedia.org/wiki/Grandfather_clause))
4. we must continuously be able to observe, audit, and intercede with the current grants and how they are being used
5. the provider of code should always be 'in control' of it, always able to stop or update the code
6. for expressive composition, we should be able to separate the grant of authority from its application
7. coupling of power and responsibility' we should be able to enforce requirements on untrusted code
8. for efficiency, code distribution should ideally be very lightweight - no need for a heavy sandbox or virtual machine.
9. a probable scenario is that thousands of clients each use identical libraries or framework code; ideally, such redundancies can be easily recognized and optimized away as if they were part of the server.
10. to protect against denial-of-service attacks, it should be possible to control and comprehend the performance costs of running untrusted code. 

RDP addresses many of those concerns. Awelon addresses a few more.

1. RDP supports [capability security](http://en.wikipedia.org/wiki/Capability-based_security) by use of first-class behaviors. Behaviors can grant limited authority over specific resources. In Awelon, these behaviors may be static or dynamic. 
2. Awelon supports [ambient authority](http://en.wikipedia.org/wiki/Ambient_authority) in some partitions for convenience reasons. However, untrusted code can be applied within logical partitions that lack this ambient authority. The type system would then constrain the code to explicit capabilities.
3. In RDP, the default state for a capability is revocation, and developers must *continuously* grant or authorize a capability by use of a signal. Thus, revocation is always possible.
4. The continuous nature of RDP's signals make it relatively easy for tools to observe and audit grants of authority (when compared to fire-and-forget message passing). RDP is also designed to support live programming, so it is not difficult to update security code and modify policies on the fly. 
5. The normal case is that disruption of the provider will also stop the code. In case of software agents that should survive disruption or perhaps even move on their own volition, developers are at least forced to use explicit state and model it carefully. 
6. Capabilities can be granted to a client independently of the code that utilizes them. This enables robust composition, e.g. where clients can further grant these capabilities to services they trust to act on their behalf.
7. Responsibilities can be enforced by use of linear types. Also, sealer/unsealer pairs can be modeled (using linear types) to enforce certain separations of responsibility. Linear types in Awelon are often static, i.e. to enforce certain proofs at compile-time.
8. Capabilities can be very lightweight. *Static* capabilities can be free at runtime. Logical partitions that lack ambient authority can also be very lightweight, even eliminated after the typecheck.
9. RDP's idempotence and commutativity properties make it relatively easy to eliminate redundant behaviors.
10. RDP has neither ad-hoc loops nor synchronous waits that make it difficult to reason about performance. Thus, once the compile-time code is expanded, it is relatively easy to reason about performance (as a function of number of clients). 

A remaining issue is that Awelon has Turing-complete compile-time metaprogramming. In practice, this can be addressed by use of simple expansion quotas, and controlling the amount of resources involved in those expansion efforts. The result is still much lighter weight than use of a virtual machine.

RDP and Awelon are very well suited for code distribution from the security perspective. Further, they are very expressive in these domains - able to easily express software agents, overlay networks, web applications that interact with cloud and client.

## Dynamic Behaviors and Live Update

## Extension and Open Systems

## Distribution, Heterogeneous Computation, and Scale


##


At runtime, RDP is mostly push-driven: updates will be aggregated into batches and pushed to the subscriber. 
 
Reactive dataflow models are similar in nature to manipulating a spreadsheet: when values are changed, downstream computations will responsively adjust to accommodate the new values. However, traditional spreadsheets are insufficient for general purpose programming: they do not effectively address state, composition, extension,  distribution, dynamic behaviors, resource management, process control, time, and side-effects. General purpose reactive dataflow models, such as  or  (and now RDP) are similar with respect to the reactive dataflow aspect, all capable of representing highly responsive real-time applications. However, these different models diverge in those secondary aspects - state, time, effects, etc.. 

RDP more broadly and effectively addresses those secondary concerns compared to any of many other programming model (not limited to reactive models) that has reached my awareness. (And there are quite a few.) 

RDP has the following properties and corresponding features:

* **bidirectional** - reactive dataflow in RDP always goes both directions: observation AND influence. For example, consider observing a camera resource for its video stream. In addition to receiving an updating video frame over time, the camera will receive a signal indicating, at the very least, that someone is observing it. This is valuable for implicit resource acquisition and management: the camera may be powered down whenever there are no observers. Rich signals might carry more information, e.g. to influence pan, tilt, zoom, focus, and other camera properties.

* **duration coupling** - the output signal from every RDP behavior is active for the same contiguous durations as the input signal. This makes process control in RDP both trivial and precise: you can always halt a complex subprogram behavior by halting the input signal. This precise process control is, in turn, valuable for dynamic behaviors, live programming, upgrade, i.e. where we swap out one subprogram for another.


RDP is an *extremely* expressive programming model, in the sense that a single RDP behavior can potentially serve many different roles: frameworks, overlay networks, software agents, applications, plugins, functions, queries, control or influence. RDP achieves this expressiveness without sacrificing performance: RDP supports implicitly batched updates and processing, logical synchronization, four layers to guard against observable inconsistency (latency, speculative evaluation, snapshot consistency, and eventual consistency), and many optimizations at multiple scales.

RDP is primarily suitable for programming of real-time control systems, distributed sensor networks, user interfaces, even interactive multi-user simulations (e.g. games). However, even for traditional 'batch' applications, RDP can offer advantages in terms of robustness, persistence, resource management, process control, live programming, and extensibility.


RDP Fundamentals
----------------

The fundamental concepts of RDP are **behaviors**, **signals**, **resources**, **partitions**, and **latency**. 

Behaviors are what RDP programmers define. A behavior describes a continuous operation that takes a signal as an input and generates a signal as output. As part of this process, signals may be sent continuously to external resources - to query or control them. Behaviors may be impure, but even impure behaviors are not stateful (i.e. you never lose information by replacing a behavior with a fresh copy of itself). The concept of 'pure' signals has some value for security and safety reasons.

Resources are external to RDP. Developers can only influence or observe a resource from afar. A

Signals are hidden within RDP. Developers never directly create or manipulate signals. Nonetheless, the conept of signals is useful when understanding behavior types or how behaviors interact with resources. Typically, a signal represents the actual or desired state for a resource, such as the position of a mouse or the contents to be displayed in a window. Signals may be *atomic* or *complex*. An atomic signal is like a time-varying integer: the integer is the smallest unit we're watching for changes, and it exists at a specific location in space-time (partition and latency). A complex signal may model concurrency (`x & y`; signals that are logically active at the same time, like a pair) or switching (`x + y`; activity switches between signals based on some upstream decision, like a union). Complex signals may be distributed in space and time.

Partitions represent *programmable locations*, often abstractly in terms of generic 'kind' - desktop, phone, cloud, client, server, CPU, GPU. Different partitions typically have access to different resources, and potentially even differ in what pure computations and types are feasible (e.g. GPU is much less flexible than CPU). An RDP application will usually start in one partition (where a 'go' signal is provided), but signals can be sent to different partitions using a "cross" behavior (like crossing a bridge), and code may further specify what should happen once the signal is received at its destination. (By design, RDP can always send the code with the initial signal.)

Partitions can also serve a few other purposes, e.g. conceptual locations like Void (from which no signal escapes), Static (for compile-time computations), or staged computing (modeling link-time or install-time as partitions). 

Latency is the 'time' concept, and is only meaningful within context of a behavior. Latency is measured relative to the start of a behavior. Latency serves a few roles. Most significantly, atomic signals cannot be combined (e.g. to add two integer signals) unless they have the same latency. (That property is essential to protect *duration coupling*.) Additionally, some signal values may be typed with use-before and use-no-earlier-than latency values, to model temporal constraints. 


RDP Patterns and Idioms
-----------------------

RDP plus even a simplistic state resource forms a Turing complete system. Any solvable problem can be solved using RDP. However, some solutions will be more or less awkward than others. Developers are encouraged to think of problems in new ways to solve them within the constraints of RDP.


