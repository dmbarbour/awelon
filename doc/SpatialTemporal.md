I want ABC to have spatial-temporal concepts 'built in', but at the moment I don't have a good way to express this in a bytecode. The issue seems to be:

* time and space coordinates shouldn't be directly exposed to ABC code. This must be privileged information (capabilities only) since code that behaves dependent upon where or when it runs raises many security concerns. 

* movement between spaces should always be an explicit effect. In some cases, it is subject to failure. In other cases, it should be controlled so we can model isolation or confinement (e.g. by creating a 'new sealed space' with relevant goto and return-from operators, and compute a block within this space).

* logical synchronization for operator seems relatively awkward to understand as a concrete behavior (within ABC's philosophy), especially for merge ( `M :: (a + a') * e → a * e`). 

Ultimately, I'm left with only a couple temporal operators that make sense as ambient authorities: 'delay' and 'expire'.

        d :: N(dt non-negative) * (x * e) → x' * e  -- x delayed by dt
        x :: N(dt non-negative) * ([a→b]*e) → [a→b]' * e -- introduce expiration

Delay makes a good ambient authority because computing takes time. Here, expiration is a substructural type limits how much further a block can be delayed. It is an error delay a relevant block beyond its expiration, or to use an irrelevant block.

However, there doesn't seem to be a role in ABC for spatial structures, unless I create an ambient authority for an orthogonal pure-space or logical-space model. Of course, there is an issue here regarding spatial idempotence; I'll need extra arguments to enter 'distinct' logical spaces. One option is to use two operations:

        h :: (Location u) ⇒ u * (x@p * e) → u * (x@{u|p} * e)
        g :: (Location u) ⇒ u * (x@{u|p} * e) → u * (x@p * e)

Alternatively, I could construct block based sealer/unsealer pairs:

        s :: (Location u) ⇒ u * e → [x@p → x@{u|p}] * ([x@{u|p} → x@p] * e)

This would closer match how capabilities are used, perhaps, but it isn't clear how I'd represent a single block of this form in ABC (except to reconstruct both and delete one, yuck). The `h` and `g` model seems better fit for ABC.

I need a good notion of what logical location actually means, some useful properties.

* Concrete spaces (CPU, GPU, etc.) should be expressible as a disciplined/constrained application of this model, with some extra effects. I.e. this model should express a simplified representation of concrete space locations.

* It should also be feasible to maintain a separation of logical spaces across physical partitions.

* There must be clear circumstances in the spatial model for which combining two values (e.g. adding two integers) is clearly a type error. It must be possible to engineer these circumstances to occur, and possible to control them. 

* It should be feasible to model 'sealed values' - i.e. such that they cannot be significantly manipulated without use of an unsealer. 

Let's see if we can find a set of constraints towards a singular design!

Point: `hg` and `gh` should always be identity if typesafe. This means:

* entry of a space cannot be idempotent or `hg` might be equivalent to `g` (if we're already there)
* exit of a space cannot be idempotent or `gh` might be equivalent to `h` (if we're not already there)

So this leaves option of counting entries like dimensions ('foo'*3) e.g. so we can seal a value more than once. If we understand these as 'dimensions' then we can also consider use of negatives to be somewhat interesting and meaningful. But it may also be useful to use only positive dimensions (negatives as a type error), or even binary dimensions (so `hh` is a type error).

The 'dimension without origin' model (w/ both positives and negatives) is actually quite tempting. Places computation in an infinite-dimensional grid instead of a tree. It would also eliminate assertions of location.

Thought: should this cover dimensionality of all things? (That would be somewhat cool, but I'm not sure it is practical.)

* Multiplying two numbers will add their dimensions.
* Adding two numbers requires they sit in the same dimension.
* Applying a block requires...? Nothing, except the sanity of the types involved.

Of course, this use of 'dimensionality' is not what I really want for 'locality'. So, I think this thought will go nowhere, unless I also wish to formalize dimensionality for numbers (which is tempting, really, but another thing I don't know how to do well right off). 

Re: commutativity of motion (is foo/bar/baz the same as baz/foo/bar?). 

### Spatial-Temporal Features

ABC models spatial properties in terms of logical partitions, and temporal properties in terms of relative latencies. Within a product or sum, the different elements may have different spatial-temporal attributes - for example, we can have types of the form `(Number@CPU * Number@GPU)`. In addition, the product or sum as a whole may have a location, with regards to where it can be introspected.

In general, information about spatial-temporal attributes is *privileged*. There are no ABC operators to query when or where a value is computed.

Additionally, a product or sum itself has a concept of spatial-temporal 'evidence' - i.e. regarding when and where knowledge that the product is a product becomes available. This is important, for example, when loading text on a remote machine.

The spatial properties are not directly accessible from ABC. Communicating between partitions is considered an effect, and is thus controlled by use of capabilities. 

Effects and their capability texts are specific to a partition. To be reusable across partitions, subprograms are written in a pure or capability-secure manner that does not hard-code any capability text (modulo annotations or references to ABC resources, neither of which are effectful). Of course, partial evaluation can specialize reusable programs, distributing capabilities at compile-time. 

The notion of logical partitions is very versatile:

* Heterogeneous systems can be modeled as partitions with different resources and effects. 
* Distributed systems are modeled by having some communication capabilities admit disruption.
* Staged programming can be modeled by modeling asymmetric communication between some partitions (i.e. different stages become different spatial partitions). 
* Confinement or purity can be enforced by computing a block in a fresh logical partition.


To perform conditional operations on a sum type requires projecting information *into* the condition, rather than the converse. This has important implications with respect to spatial properties, e.g. ABC can typefully enforce that a condition computed on the GPU is not accessible for decisions on the CPU. 


 Naturally, ABC code containing embedded capabilities is almost never polymorphic; reusable ABC code is either pure or explicitly models distribution of capabilities. Distributed systems are modeled in terms of communication capabilities that admit disruption or failure. 

There are no primitives to communicate between partitions.

Spatial-temporal information is considered privileged. That is, without a dedicated capability, an ABC subprogram cannot ask *where* or *when* it is running. 


As a general rule, access to spatial-temporal information is considered privileged. That is, an ABC subprogram cannot vary its behavior based on *where* or *when* it is running, unless it is explicitly granted that information or has a special capability to acquire it. 



*ASIDE:* For RDP, the convention for modeling disruption is to first model acquisition of a connection, which may fail, then to treat the connection as reliable. This separates failure handling from the acquisition code.

In a 

 The separation is useful. 

 This works well in a reactive model, since the acquisition may reactively fail or recover over time.

 Heterogeneous systems can be modeled as partitions with different resources and effects. Distributed systems can be modeled by having some communication capabilities model disruption.

 allow disruption. RDP can leverage reactivity by separating acquisition of a st

        getConnection :: something → (fail + connection)
        connection is 





There are no primitives for communication between partitions. 

ABC has no primitives for communication between partitions. That is, communication between partitions requires an explicit capability. Distributed systems are generally modeled in terms of communication capabilities that allow disruption.

every point-to-point communication requires a capability.


*NOTE:* Even pure ABC can be incompatible with physical constraints of some partitions. For example, if a partition represents a GPU shader, not every ABC program can be compiled to a valid shader. In these cases, we might accept partiality, that a compiler may reject some well-typed programs because it doesn't know how to translate them. Alternatively, we might model a DSL within ABC.

that we can prove is safe.


This is essential for RDP: a single RDP behavior can model reactive overlay and orchestration networks that interact across heterogeneous servers, clients, CPUs and GPUs. However, spatial-temporal features are useful even for imperative code as a basis for precisely reasoning about concurrent behavior, mobile code, consistency, progress, and scheduling. 

A compiler for ABC might break a holistic program into shards that maintain behavior in each partition.

I'll consider this in two parts: temporal attributes, and spatial attributes.




* location and latency properties model where and when values can be accessed
* latency constraints for blocks and sealed values - expires, ripens

Absolute latency should never be observable. But maybe can compute difference of latency difference between two values. OTOH, computing latency difference in a dynamic scenario would logically require waiting. So maybe computing difference in latency doubles as a synch operation? That could be useful.

Should 'synch' be primitive? Not so sure... maybe? I want latencies to be easy to reason about, including equality of latencies. 

multi-parameter operations
spatial properties of sums and products

Location values are not observable, except by capability.


Logical latency properties aren't just for type safety. They guide the scheduler, and support logically synchronous actions on distributed objects, which may result in synchronized behavior on hardware if the hardware supports precise buffering and timing.

 for atomic values (numbers, blocks). Logical latency is a rational number, indicating a time in seconds. Blocks may also have latency constraints on when they can be invoked. Logical latency is only increased by a logical delay operator. Logical delay simply increments logical latency. 

The relationship between logical latency and real-time is maintained by a scheduler. A good scheduler will keep logical and real time tightly aligned with predictable failure modes, using both soft and hard mechanisms, and some scheduling may occur at compile time. If an effect is invoked on the future, it may be scheduled without invoking it immediately while computation continues elsewhere. Or if a computation is running ahead of where it needs to be, the scheduler may devote more resources to other computations.

The logical model of time, especially on a real timeline (seconds, not arbitrary units), is valuable for understanding and controlling feedback behaviors, for achieving consistent behavior for reactive networks overlays, for comprehending interaction of concurrent effects. However, developers don't always need to think about time. In many cases, the role of assigning temporal properties can be pushed into other layers - networking, effects, frameworks.


### Sealed Values (preliminary)

The notion of sealers, unsealers, and sealed values is very useful for modeling ADTs, encapsulation, identity, security patterns, and rights amplification. ABC is designed with the expectation that developers will leverage this pattern to address many challenges traditionally handled through module systems.

In pseduo-types:

        type Sealer U = Value → Sealed U Value
        type Unsealer U = Sealed U Value → Value
        type NewSealerUnsealer = U! → (Sealer U, Unsealer U)
            where U! describes an affine uniqueness source

 value and return a sealed value, specific to the sealer
* an unsealer will process a sealed value from the corresponding sealer

a capability that takes a value and returns a sealed value
* unsealer: a capability that takes a sealed value from the corresponding sealer, and returns the underlying value (no longer sealed).



A new sealer/unsealer pair can only be constructed through a capability that takes a unique value as an argument. (Uniqueness can be enforced by substructural types.) Anyhow, there is no primitive to create a sealer/unsealer pair, nor is there any primitive source of uniqueness. The sealer/unsealer concept requires support from the environment. 

From these, it is easy to also construct a capability that will unseal a value, apply a block to it, then seal the result up again. 

Anyhow, sealed values greatly benefit from recognition by type systems and optimizers. Especially of interest is what it might mean to seal a value that is distributed across space or time. It isn't clear, at the moment, whether any ABC primitives would help out, or whether some convention (in the naming of sealers and unsealers) would be sufficient. A variation on sealers/unsealers is to create a 'sealed space' - a new 'partition' for values which has a new set of capabilities for entry and exit. This could be useful for enforcing that a subprogram is typed to operate in any space.

I plan to return to the issue of systematically modeling sealed values at a later time.



