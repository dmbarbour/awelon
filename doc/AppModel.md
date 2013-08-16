(in a state of slow destruction)


Reactive Demand Effects
-----------------------

The nature of effects in RDP is simply that *a system's state may be influenced by its observers*. 

For example, if we query a drone for a good picture of a particular zone (specifying the zone in the query), the drone may volunteer to point its camera in that direction, or even divert its flight. This is possible due to the bidirectional dataflow of RDP: the query itself delivers information upstream about what is desired, and that information may be acted upon. Of course, it may be that our goal is to influence the behavior and we are not concerned about the response. In that case, the query is closer to a control signal and is said to express *demand*.

RDP is a dataflow model. Queries are continuous, long-lived, and may be modeled by subscriptions. Demands have the same properties - one can understand them as publishing values that influence targeted subsystems. If modeled in a publish-subscribe system, RDP would tightly couple every subscription with a published value. The published value may also be updated over time, modeling time-varying queries or demands. Thus, dataflow in both directions may be reactive, varying in time.

A system's state may be continuously influenced by the *set* of active demand values on that system. 

The word *set* is used in a strict sense: duplication or ordering within the set must have no semantic effect. This constraint is the foundation of RDP's equational reasoning properties (causal commutativity and spatial idempotence). These properties contribute to the declarative programming experience, and further enable many optimizations. Locally, the optimizations include stream fusion and pipline parallelism. Globally, optimizations include ad-hoc proxy caches or mirroring. If developers have a requirement to distinguish demands, for example to model a vote, they must do so by coupling each demand with a unique identifying token.

Commutativity and idempotence enable code to be rearranged, duplicated, and duplicates eliminated with a great deal of freedom. Refactoring impure RDP code is very similar to refactoring pure code, excepting that impure behaviors cannot be fully eliminated when the response is unnecessary. But pure code can be advantageous when reasoning about security. Awelon's type system can allow specification of pure behaviors.

External State
---------------

State is always just beyond the outer edge of an RDP system, in some abstract database. With external state, orthogonal persistence comes naturally, as does smooth handoff when updating code. Open extension is always feasible - i.e. we can always introduce additional behaviors to observe or influence the state. State may still be secure and modular, e.g. through cryptographic identity (HMAC, PKI, sealed values, sparse caps) or secure partitioning (chroot, sandboxing). But state isn't encapsulated.

State is assumed to be an *abundant* resource by most applications, meaning they have as much state as they want. It is only necessary that each state resource be specified by a distinct identifier (much like filenames in a filesystem). It is not difficult to develop schema that ensure different entities and relationships are associated with different states. 

However, which state models are available depends ultimately on the API provided to the application object. It would not be difficult to enforce a quota through the type system (e.g. by providng only a few finite-state resources).

State resources for RDP are different from those from imperative systems. Since demands are continuous, eventful update models do not make sense - e.g. what exactly does it mean to "add one" continuously? And there is no serialization or exclusive control: state must be influenced by the full *set* of concurrent demands. However, there are plenty of possible state models. For example, a pair with an initial state and an update behavior `(s0, Set w -> s -> s)` can specify a lot of simple discrete-varying, time-insensitive state models. Richer models might take information about time, or might be animated: updating even while demand stays constant.

It is feasible to allow developers to specify ad-hoc state models. This is effectively achieved by augmenting the identity of the state resource with information about its behavior. RDP cannot create 'new' state, but if state is *abundant* then there may be states of many kinds. A capability to access this ad-hoc state may be provided to the application object.

Awelon programmers can generally assume that state is persistent (unless it's naturally volatile) and I may pursue standard support for ad-hoc state.


Temporal Properties
-------------------

RDP has a number of explicit temporal properties that are implicit in most other paradigms.

### Logical Latency

Spreadsheets offer an illusion of *instantaneous* communication, which is feasible due to their locality and stateless nature. RDP cannot offer the same. It takes time to query remote resources. It takes time to perform computations. If we happen to express an open feedback loop (where a response influences demand influences response) then time is essential to understanding and modeling the loop. A feedback loop is a form of temporal recursion (always with an external resource). 

RDP models logical latency as a static property. In Awelon, this is modeled in the type system. Every value has a logical latency, a time at which it (logically) becomes available. The logical latency is often a conservative estimate, or based on real-time requirements, though it can potentially be based on empirical metrics.

Note that *logical latency* is distinct from an actual, implemented, *physical latency*. In practice, if logical latency is much larger than physical latency, this corresponds to buffering and may result in physical delays. And if physical latency runs higher than logical latency, there may be retroactive updates. 

Awelon programmers may inject latency into their code, or may rely on implicit latency from effects handlers. The most essential place to inject latency is for feedback loops. 

### Duration Coupling

An invariant of RDP is tight duration coupling: if you query a value over a continuous period of exactly 43.2 seconds, you will also receive a response signal over a continuous period of exactly 43.2 seconds. There may some latency between issuing the query and receiving the response, but it will be statically known and typically much smaller than the durations (e.g. 30ms). 

Relevantly, even if the query result is not immediately available, or there is an error, you will still receive a response for the full duration you maintain the query. But the response may need to include an error option, or the ability to indicate one is waiting on the full result. (A 'waiting' response is a trivial way to integrate asynchronous processes with RDP. A more interesting option is to provide partial results.)

Duration coupling enables RDP developers to reason about resource control and process control. In particular, programmers know that if they *stop* sending the signal, then the next behavior in the chain will also *stop* sending the signal, inductively all the way through the behavior. Resources tend to take advantage of this by returning to a passive or low-power idle state when there is no active demand. Developers rarely need to concern themselves with explicit cleanup.

### Logical Synchronization

### Speculation

Spatial Properties
------------------

### Location Types

### Heterogeneous Spaces

### Crossing

### Signal Distribution and Disruption

Distributed Programming
-----------------------

