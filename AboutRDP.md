
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

Due to variation in logical latency on different code paths, there may be times where active durations for `(x + y)` overlap, and times where durations for `(x * y)` do not overlap. In this sense, composites are logically asynchronous. 

Asynchrony with static, logical latencies is qualitatively different than what most programmers consider asynchronous. I think the "update at different rates and times" feature for composite signals is a closer fit to what is popularly considered asynchronous. RDP can also easily model asynchronous communication by use of shared state.

Signals must be synchronized for some operations. For example, to take the maximum of two integer signals requires both signals be in the same partition and have the same latency - they must coexist at the same space and time. Awelon will provide a `synch` operation that smartly synchs the different inputs by introducing a minimal set of delays.

#### Continuous Signals

Most signals in RDP systems are discrete-varying, i.e. jumping from one value to another at particular instants in time. But true contnuous-varying signals are valuable for modeling animations, collisions, sound, and other physical phenomena. Unfortunately, arbitrary continuous-varying signals can be too expensive to compute. Fortunately, we can often benefit even from approximations of physical signals, so long as we can control the error. 

Potential models for continuous signals include polynomials, trigonometric polynomials, time-varying splines and surfaces. I would generally favor models that can be expressed as an array or matrix, either of coefficients or control points, and that would lend themselves to simple time-shift (`delay`), splitting on zero-crossings, and computations on a GPU (using CUDA). Operations on continuous-varying signals may generally be constrained. 

(So far I haven't implemented continuous-varying signals in any RDP system. They're on the todo list.)

### Resources

A **resource** is external to RDP yet open to programmatic observation or influence. Resources broadly include sensors, actuators, and state. Specific resources might be mouse, keyboard, video displays, speakers, robotic arms, motors, physical storage, and virtual resources such as files, canvases, or subwindows. An RDP system must provide some behaviors, as primitives or capabilities, to access external resources. The biggest challenge in making RDP useful is integrating a wide variety of resources. 

Fortunately, RDP signals are a good fit for most sensors and actuators, and it is not difficult to invent RDP-compatible state models. Unfortunately, there are too many sensors and actuators to integrate directly, and adapting existing state resources can be awkward. The approach under consideration is to first support web application services for UI, then to integrate a pubsub model (likely [ROS](http://www.ros.org/wiki/) or [DDS](http://portals.omg.org/dds/)) to help integrate the broad variety of physical devices. After that, specific resources can be integrated with RDP to improve quality of service (temporal precision, latency, speculation, etc.).

#### State Resources

An important part of an RDP system is a useful pool of state resources - i.e. resources whose entire purpose is to accumulate information from the past. These resources should be *abundant* - i.e. applications use a finite set, but that finite set is as large as they need. A distinct identifier is provided for each state resource, much like filenames in a filesystem. By use of substructural types to enforce uniqueness, these resources may also be *exclusive*, offering benefits (implementation hiding, local protection of invariants) comparable to encapsulated state.

RDP is carefully designed so that it cannot accumulate state internally. This property is critical for transparent switching between dynamic behaviors, valuable for security and extensibility, and convenient for orthogonal persistence or upgrade. Stateful resources cannot even be *modeled* entirely within RDP; to model a sensor or actuator requires a state resource to do the modeling. One valid concern is that: if all state resources are external to RDP, can developers define their own state resources that operate in problem-specific ways? Fortunately, this concern can be addressed in a number of ways, and developers can choose what is most convenient for themselves:

1. Some external state resources are very expressive, enabling developers to model one state resource within another. Similarly, multiple simple state resources can often be used together to model ad-hoc systems.

2. It is possible to carry an ad-hoc state model in the "identifier" for non-exclusive state - e.g. looking for a state resource that models a particular state-machine expressed as a regular expression. This approach a weakness: it is not stable across code updates that would tweak the state machine. However, this is an acceptable weakness for some problem domains.

3. For exclusive state (via substructural types), we can couple an ad-hoc state model to a unique identifier. We can also stabilize the unique identifier across source-code changes, e.g. by using the uniqueness type to uniquely allocate names in a filesystem-like directory. The state model may contain instructions on how to upgrade from a prior version.

The advantage of the latter two options is that they provide more *static* knowledge about the behavior of the state. This is valuable for performance, and for reasoning about system behavior. 

Potential state models include, but are not limited to:

* state machines, which react to applied signals
* windowed histories - report last N seconds of inputs
* tuple spaces variants - write, read, erase, expire tuples
* term rewrite variants - state is a term; signals provide rewrite rules
* animated term rewrite - some terms change automatically over time

It is not difficult to create new state models suitable for RDP. And these state models can be very expressive, even Turing complete on their own: animated term rewriting, for example, is excellent for bridging RDP systems with imperative systems (especially if some terms represent procedures). However, state models in RDP systems should be isolated: influence of one state upon another should always be expressed through an RDP behavior. 

State in RDP systems is persistent unless there is a natural excuse for it to be volatile. A short-lived rolling window of history, or a tuple space where tuples expire after a couple minutes, could easily be volatile. But persistence by default is a convenient property. (*Note:* Animated state should continue to be animated while persisted; well designed state must either go passive or enter a predictable loop in absence of influence.)

#### Distributed Resources, Heterogeneous Computation

Resources are not 'globally' available in an RDP system. Instead, they are coupled to logical partitions. (And those logical partitions are generally coupled to physical partitions.) In order to observe or influence a resource, the developer must first figure out how to route a signal to the appropriate partition. In some cases, routing may be ad-hoc; in other cases, it is asymmetric. In some cases, connectivity is guaranteed; in others, developers must deal with disruption. This goal isn't to stump developers, but rather to more accurately reflect connectivity relationships in the world, resulting in a more robust and efficient system. 

Different resources - sometimes even different collections-processing or numerical behaviors - can result in a very heterogeneous computation environment. To help with this challenge, Awelon provides rich metaprogramming, e.g. the type system can track which partitions are in use, and developers can automate some in-the-large computations to find appropriate implementations. 

*NOTE:* Partitions are useful for security, e.g. some kinds of partitions might represent sandboxes with no [ambient authority](http://en.wikipedia.org/wiki/Ambient_authority), for which the only escape is explicit capabilities obtained from another partition.


### Behaviors 




## Miscellaneous

### Speculation and Anticipation

A resource's state is *formally* determined by its current state and the set of signal values influencing it at the logical instant. However, resources are not forbidden to predict future input signal values and adjust or prepare accordingly. Doing so is often good sense. For example, if we anticipate that a particular file resource might be needed, we can load it a little in advance. In the worst case, we wasted some CPU and time. If our predictions are accurate often, the system can be more efficient (making better use of what might otherwise be idle cycles) and more responsive.

Naturally, this sort of anticipation would be greatly improved in precision, accuracy, and even safety if we could actually peek at our future input signals. RDP is designed to support this pattern pervasively: 

* signal updates can carry information about speculated future values
* conservative use of `delay` introduces natural time-buffers
* resources can speculate their own future states to keep it compositional
* signal updates very often apply to the future of the signal, keeping present stable

The result is a system where the future is in a state of constant flux, but the present and recent past are stable or are rapidly stabilizing. This time information is also useful for focusing computation resources on the updates that are lagging behind, and for protecting against runaway speculation (deep-future speculation can be very idle in nature).

This form of speculation enables RDP systems to be very responsive, predictable, and fair. RDP systems will 'gracefully degrade' when overloaded by slowly shifting speculative computations towards more critical ones. To the degree that speculation is often accurate, it can mitigate hiccups in network connectivity or scheduler latencies, resulting in a more robust systems. There are also efficiency benefits of signals carrying future values: it supports 'temporal batching' where a single update package can carry multiple future values, and it supports 'batch composition' where multiple batches can be composed then processed together (by combining individual signal updates). 

Speculation is very useful for open composition of on-the-fly planning loops and prediction models.

I believe speculation and anticipation will be one of RDP's "killer features". 

### Code Distribution in Mutually Distrustful Systems

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

### The Void

In RDP, duration coupling means our output signals must always match our input signals in duration. However, those signals don't always need to be accessible. Most RDP systems will support what is effectively a `/dev/null` partition called "void". Signals that enter void cannot escape; everything in the void can be eliminated as dead code. In most cases, of course, it would be a type error to throw linear types into the void.


### Stateful Communication

There is always a temptation to use an external resource as a bypass, a way to communicate outside the lines and primary structure of the language. In most cases, giving into this temptation is a bad thing.

Awelon mitigates this temptation in two ways. First, Awelon supports ad-hoc data plumbing by use of hands or named stacks: when a signal is needed, a developer can get to it and take or copy the signal, often with less effort than it would take to set up shared state. Second, Awelon supports promises by use of fractional types: developers can "promise" a signal into existence then later fulfill it. 

However, stateful communication can be useful. And RDP is good at it.

Stateful communication is useful for:

* occasionally offline systems and disruption tolerance (like e-mail) 
* open systems where the audience is unknown (like a website) 
* open systems where the contributors are unknown (like a bulletin board)
* cases where we must time-share limited resources - e.g. we need task queues for printers
* cases where we don't know how long work will take - e.g. for ray-tracing or searching a filesystem

RDP is good at stateful communication because:

* vigilantly observe for changes in state, react immediately
* easily observe *multiple* states, react when all are appropriate
* easily support multiple observers, without update ordering issues
* support for speculative evaluation of states; anticipation of change
* external state enables orthogonal persistence by default

RDP was initially designed to support [publish-subscribe](http://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern) systems in a resilient, consistent, scalable manner. RDP is effective for integrating collaborative state: [blackboard system](http://en.wikipedia.org/wiki/Blackboard_system), [tuple spaces](http://en.wikipedia.org/wiki/Tuple_space), or even modeling ad-hoc [workflow patterns](http://en.wikipedia.org/wiki/Workflow_patterns).

Awelon further augments RDP by supporting exclusive state via the uniqueness source. Developers can enforce that certain state is one-to-one, one-to-many, or many-to-one. 

### Modeling Events

Events in RDP are awkward and discouraged. RDP does not directly express 'instantaneous' events. There are [many reasons](http://awelonblue.wordpress.com/2012/07/01/why-not-events/) for this, but those reasons don't make events any less awkward. Events might exist due to integrating with certain event-triggered sensors, or modeling event-based simulations or video-games. 

In general, a sensor input event will be modeled as a short-lived signal, ideally based on actual timing properties. But if not, just choosing a fraction of a millisecond will often be sufficiently fine grained. RDP's dataflow is not lossy, so even a short-lived signal will be processed and integrated into state in a consistent manner. 

To model events in a stateful system is easiest if you can lift the event into some sort of intermediate state for integration. E.g. create a tuple representing that a character was entered in a text area, and allow that tuple to be integrated with the text by some other agent.

If that doesn't work, events can always be modeled with a state trick: 

* if the event is not recorded as having been delivered, we deliver
* while delivering, after small delay, we record event as delivered
* old events are cleared from history after a few seconds

If there is some sort of event acknowledgement, we should prefer that instead of a local "I sent it therefore it was delivered" short-circuit. The 'small delay' is very important for a consistent system. A similar technique can also be achieved using animated state, or possibly a clock.

#### Demand Monitors




## Dynamic Behaviors and Live Update

## Extension and Open Systems

## Distribution, Heterogeneous Computation, and Scale


