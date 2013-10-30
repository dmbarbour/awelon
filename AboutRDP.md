
# Reactive Demand Programming

Reactive Demand Programming, or RDP, is a reactive dataflow programming model designed for orchestrating open systems. RDP is suitable for modeling overlay networks, multi-agent systems, shared services, and first-class frameworks. 

RDP differs from other reactive dataflow models - such as [synchronous reactive programming](http://en.wikipedia.org/wiki/Synchronous_reactive_programming) and [functional reactive programming](http://en.wikipedia.org/wiki/Functional_reactive_programming) - in how RDP addresses the concerns of side-effects, state, security, resource management, distribution and scale, extension and open systems, and live update or upgrade. 

In practice, RDP is augmented with [linear types](http://en.wikipedia.org/wiki/Substructural_type_system) to address RDP's relatively few weaknesses. 

At runtime, RDP is push-based after establishing a relationship. Fine-grained signal updates are aggregated into batches that may be sent to another thread or remote system. Updates are often speculative and subject to change, reporting expected future values and correcting as needed. Fine-grained (per signal) speculation and retroactive correction enable high levels of optimistic parallelism and some very nice consistency properties (cf. [time warp protocol](https://www.google.com/search?q=time+warp+protocol)). 

RDP is designed for code distribution in mutually distrustful systems. I.e. some service interfaces should allow for signals containing code. Awelon Bytecode (ABC) is intended to be the primary distribution language. RDP and ABC address many security concerns that would otherwise hinder code distribution (see below).

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

Due to variation in logical latency on different code paths, there may be times where active durations for `(x + y)` overlap, and times where durations for `(x * y)` do not overlap. In this sense, composites are logically asynchronous. 

Asynchrony with static, logical latencies is qualitatively different than what most programmers consider asynchronous. I think the "update at different rates and times" feature for composite signals is a closer fit to what is popularly considered asynchronous. RDP can also easily model asynchronous communication by use of shared state.

Signals must be synchronized for some operations. For example, to take the maximum of two integer signals requires both signals be in the same partition and have the same latency - they must coexist at the same space and time. Awelon will provide a `synch` operation that smartly synchs the different inputs by introducing a minimal set of delays.

#### Continuous Signals

Most signals in RDP systems are discrete-varying, i.e. jumping from one value to another at particular instants in time. But true contnuous-varying signals are valuable for modeling animations, collisions, sound, and other physical phenomena. Unfortunately, arbitrary continuous-varying signals can be too expensive to compute. Fortunately, we can often benefit even from approximations of physical signals, so long as we can control the error. 

Potential models for continuous signals include polynomials, trigonometric polynomials, time-varying splines and surfaces. I would generally favor models that can be expressed as an array or matrix, either of coefficients or control points, and that would lend themselves to simple time-shift (`delay`), splitting on zero-crossings, and computations on a GPU (using CUDA). Operations on continuous-varying signals may generally be constrained. 

(So far I haven't implemented continuous-varying signals in any RDP system. They're on the todo list.)

### Resources

A **resource** is external to RDP yet open to programmatic observation or influence. Resources broadly include sensors, actuators, and state. Specific resources might be mouse, keyboard, video displays, speakers, robotic arms, motors, physical storage, and virtual resources such as files, canvases, or subwindows. An RDP system must provide some behaviors, as primitives or capabilities, to access external resources. 

In RDP, a resource's state is determined from its previous state and the set of signals influencing it at the logical instant. By "set" I mean that ordering and duplication should not affect state. (When duplication is relevant, it must be modeled explicitly by adding a GUID or similar to the signal.) This constraint is how RDP achieves idempotence and commutativity.

The biggest challenge in making RDP useful is integrating a wide variety of resources. Fortunately, RDP signals are a good fit for many sensors and actuators, and it is not difficult to invent RDP-compatible state models. Also, RDP is effective for integrating existing time-series publish-subscribe systems (albeit, with some cost to speculation). A viable approach to integrating useful resources quickly is to focus early on existing publish-subscribe systems - e.g. [ROS](http://www.ros.org/wiki/), [DDS](http://portals.omg.org/dds/), or [MQTT](http://en.wikipedia.org/wiki/MQ_Telemetry_Transport).

#### State Resources

Many applications require long term stateful storage. In RDP, this is achieved through binding external 'state resources'. Despite being external, state can initially be exclusive if bound via linear types, thus offering the benefits of secure encapsulation. To further stabilize the binding of state resources against future source-code changes, developers are also required to uniquely identify resources using text in the code.

Since state is formally external to RDP, and also structured hierarchically, state is naturally accessible for orthogonal persistence, auditing, debugging, freeze, rewind, rewrite, or restart.

State resources suitable for RDP must be controlled by a signal or set of signals, which results in very different state models compared to imperative code. Potential state models include, but are not limited to:

* state machines, which react to applied signals
* windowed histories; report last N seconds of inputs
* exponential decay of history; full history with limited fidelity
* tuple spaces variants; write, read, erase, expire tuples
* term rewriting; state is a term, signals provide rewrite rules
* integrals of inputs over time

It is not difficult to create new state models suitable for RDP, and programmers can define their own state models when using exclusive binding. (Awelon project uses exclusive binding for most state resources.)

RDP is designed to eliminate common requirements for non-essential state. However, state is sometimes useful for communication, for example when the reader and writer are active at different times, or for integrating resources that can handle only a limited set of problems at a time (e.g. a printer queue). RDP is good at this. The ability to continuously observe and influence state is very convenient for expressing ad-hoc [workflow patterns](http://en.wikipedia.org/wiki/Workflow_patterns).

#### Distributed Resources, Heterogeneous Computation

Resources are not 'globally' available in an RDP system. Instead, they are coupled to logical partitions. (And those logical partitions are generally coupled to physical partitions.) In order to observe or influence a resource, the developer must first figure out how to route a signal to the appropriate partition. In some cases, routing may be ad-hoc; in other cases, it is asymmetric. In some cases, connectivity is guaranteed; in others, developers must deal with disruption. This goal isn't to stump developers, but rather to more accurately reflect connectivity relationships in the world, resulting in a more robust and efficient system. 

Different resources - sometimes even different collections-processing or numerical behaviors - can result in a very heterogeneous computation environment. To help with this challenge, Awelon provides rich metaprogramming, e.g. the type system can track which partitions are in use, and developers can automate some in-the-large computations to find appropriate implementations. 

### Behaviors 

A behavior is the basic code unit of RDP. The input and output for a behavior is a signal. Many behaviors are pure functions on the input signal. Some special behaviors - capabilities - can also access resources, to continuosly observe or influence them. 

A behavior is said to be 'active' when its input signal is active. RDP enforces an invariant called **duration coupling** - the active periods of the output signal must match the active periods of the input signal, modulo an increase in latency. Duration coupling is very useful for resource and process control, i.e. downstream resources can always be released by stopping the behavior upstream.

Behaviors in RDP are naturally composable, i.e. the output of one behavior can be fed wholly or partially as the input to another. 

## Miscellaneous

### Speculation and Anticipation

A resource's future state is determined by its current state and the set of signal values influencing it at the logical instant. Assuming we can speculate the future value of each signal, we can potentially speculate the future states of a resource. In RDP systems, wherever possible, this speculative future should be communicated and updated as needed. The goal is to achieve speculation as a compositional feature. 

This enables RDP systems to anticipate the future with a fair degree of accuracy, which can greatly improve a system's responsiveness - i.e. even when our predictions aren't precise, they might be 'good enough' that we need relatively few corrections to our plans. If we speculate some heavier computations in advance (during what might otherwise be idle cycles) we should have more CPU available for any minor last-millisecond updates. 

The system as a whole becomes more robust to brief hiccups in network, scheduling, or burst communications. If the system falls behind a little, the speculated value will often be 'good enough' in the short term. RDP systems can 'gracefully degrade' by scheduling time-critical computations with greater priority than speculative computations. Also, multiple updates on the future can be batched together, to gain some extra batching efficiencies. 

Anticipation can also result in smoother transitions. E.g. if we anticipate the motions of a robotic arm, we can implicitly rotate the joints into to the ideal locations for the next action. Or if we anticipate which textures we need, we might pre-load them onto the GPU. Speculation is extremely useful for open composition of on-the-fly planning loops and prediction models. 

I believe speculation and anticipation will become one of RDP's "killer features". 

### Code Distribution in Mutually Distrustful Systems

RDP is designed for automatic code distribution. Code distribution is useful for reducing latency and bandwidth overheads, for making systems more disruption tolerant, for modeling overlay networks and distributed frameworks. Historically, we've accepted asymmetric code distribution from server-to-client, but we've been reluctant to accept the other direction due to security reasons. 

RDP helps address these security concerns. First, RDP assumes [capability-based security](http://en.wikipedia.org/wiki/Capability-based_security). This enables developers to precisely control distribution of authority and reason about it compositionally. Second, capabilities in RDP are distributed in a continuous, reactive manner, and they are implicitly revoked once they aren't explicitly shared - this is a significant boon for keeping security properties visible and maintainable. Third, RDP has explicit latency properties that can help guide and control how much computation should be permitted for code, mitigating risk of denial-of-service when executing untrusted code.

RDP is also designed to help with flash-crowds. In many cases, we can assume that hundreds of clients will use similar code, and often similar signals. RDP's idempotence properties enable significant optimizations in these cases, similar to content distribution networks. 

In Awelon project, capabilities in ABC code can often be compiled statically into the application. I.e. we can have the benefits of object capability model without the performance overheads of pointer indirection.

### Modeling Events

Events in RDP are awkward and discouraged. RDP does not directly express 'instantaneous' events. There are [many reasons](http://awelonblue.wordpress.com/2012/07/01/why-not-events/) for this, but those reasons don't make events any less awkward. Events might exist due to integrating with certain event-triggered sensors, or modeling event-based simulations or video-games. 

There are at least two reasonable approaches to modeling events:

* events as short-lived signals
* stateful log of recent events

The latter option could be implemented above the former, but might be more friendly to newcoming observers.


