# Spatial-Temporal Features?

I intend that Awelon project track spatial-temporal attributes - location and latency - for values. But it isn't clear to me how this should be expressed in the ABC code, and whether it should instead be pushed to capability invocations. 

It turns out that, to protect composition and decomposition properties, it must be possible for any block to receive from and send to multiple locations. The decomposition property is that: `[foobar] = [foo][bar]o` for any break between complete subprograms. But if I manipulate space for different elements in a product and I require single output location, then a lot of breakdowns would be unsafe and it would take a lot of global context to determine which ones are safe.

However, 'location' has its own issues with the `$` operator. How can I receive arguments at multiple logical locations if, for example, the block itself is at just one location? How do I specify these locations must have some aspects in common?

I need a more compositional (perhaps monotonic?) model for space...

A related, significant challenge is spatial-temporal attributes for literals. It seems to me the most natural location for a literal is the 'block' (or 'stream') of which it is part. This is okay, I think, but it could create a few hassles with regards to tracking 'expiration' and interacting with objects in various physical spaces.

Until I address these challenges, I cannot really model spatial-temporal attributes at the ABC layer. I can, however, model them using capabilities.

Regarding my current efforts:

Likely: logical delay, expiration of blocks

Possible: logical/pure space manipulators

Challenges: 

* where and when are literals? 
* should I focus on staging? 
* can I model sealer/unsealer pairs? 
* should delay or synchronization be implicit?
* can I apply blocks that operate on multiple locations?

Related: 

* [Computing Needs Time](http://www.eecs.berkeley.edu/Pubs/TechRpts/2009/EECS-2009-30.pdf), Edward Lee 2009. 
* [Type Theory for Mobility and Locality](http://www.cs.cmu.edu/~jwmoody/doc/talk/slides-proposal.pdf) by Jonathon Moody.

### Parametricity?

I would like to enforce parametricity in some cases. But ABC's "spatial idempotence" feature forbids this from being readily expressed by operators. The closest I know how to model would be an operator on a block, `p`, that somehow enforces parametricity, constraining behaviors on certain inputs. However, ABC doesn't really have the ability to express that a condition must hold 'forall' inputs of a certain class. I suppose this must be expressed using property tests. 

# Older Stuff


ABC values have spatial-temporal attributes, logical location and latency. Location can distinguish concepts like whether a number is on the client or server, the CPU or GPU. Latency tracks when values become available, now or later. 

A value can be delayed by the operator `s`, which takes a rational numbner of seconds.

        s :: N(non-negative dt) * (x * e) → (x' * e)

Latency is *logical* in the sense that actual, physical latency can diverge from logical latency. With speculative evaluation, physical latency can be lower, e.g. to maintain a buffer of future actions. If computation resources are overburdened, physical latency can be higher. However, logical latency guides physical latency and scheduling of side effects. In most cases, they will match with a high degree of precision.

Not every value may be delayed without consequence. In particular, ABC has a concept of *expiration* as a substructural type for blocks.

        t :: N(non-negative dt) * ([a→b] * e) → [a→b]' * e

Expiration is recorded as a rational number of seconds relative to latency of the block. If delayed beyond this value, the block expires. An expired block cannot be applied by `$` or `?` operators. If a block is also *relevant*, then expiration of that block is a type error. Between latency and expiration, developers can model upper and lower bounds for temporal behaviors.

Logical location is represented in a multi-set. I use `x@p` to describe a structure of type `x` having location `p`. ABC provides operators `n` and `x` to enter and exit a location. 

        n :: (Dim u) ⇒ u * (x@p * e) → x@{u|p} * e
        x :: (Dim u) ⇒ u * (x@{u|p} * e) → x@p * e
        -- Dim is usually text - copyable, droppable, comparable

One may enter a space more than once, but it is not legal to exit a space that has not been entered. By itself, logical location can serve a role similar to newtype - i.e. protecting against accidental interactions. But this feature is most expressive when used with blocks that encapsulate a parametric or existential location. Motion becomes a securable capability. By such means, concrete locations are modeled as a subset of logical locations.

(Can I somehow enforce parametricity?)

Spatial-temporal attributes have a pervasive and somewhat ad-hoc effect on type analysis.

Operators that combine values - e.g. `+`, `*`, `>`, `o` - are not valid unless both arguments coexist in the same space. If they have different latencies, the resulting latency will be the maximum of the two. The `$` and `?` operators require delaying every argument at least to match the block, and cannot be used if the block has a very different location than the arguments. 



It might be problematic for composition if outputs can have multiple locations but inputs cannot. OTOH, if I limit it to a block having one input location and one output location, the compositional implications are more obvious... but there are also more constraints on decomposition. 

I would like to maintain the tacit concatenative structure such that, any complete subsequence is a valid subprogram. This will be seriously damaged if I require single input or single output locations for blocks, simply because certain subprograms - e.g. that change the location of just part of the input - are not valid for use in blocks. 



But this might be protected so long as I insist that blocks apply at a single location.


Where is a static block? Literals require special attention. Where and when are they? How much effort should it be, to add one to a value at a given location?

I feel that, rather than locations, I need some sort of 'evidence' model - the ability to reproduce a value at a given location. Static values are easy to reproduce; dynamic values not so much. OTOH, it does seem reasonable to require that blocks operate at a single location.

# Even Older Stuff

I want ABC to have spatial-temporal concepts 'built in', but at the moment I don't have a good way to express this in a bytecode. The issue seems to be:

* time and space coordinates shouldn't be directly exposed to ABC code. Any information about 'absolute' locations must be privileged information (accessible via capabilities only). Code whose behavior depends upon where or when it runs raises many security concerns. 

* movement between concrete spaces should always be an explicit effect. In some cases, it is subject to failure. In other cases, it should be controlled so we can model isolation or confinement (e.g. by creating a 'new sealed space' with relevant goto and return-from operators, and compute a block within this space).

* logical synchronization for operator seems relatively awkward to understand as a concrete behavior (within ABC's philosophy), especially for merge ( `M :: (a + a') * e → a * e`).

Implicit synchronization seems problematic for block expiration, but it might not be; it would be worthwhile to determine this.

Ultimately, I'm left with only a couple temporal operators that make sense as ambient authorities: 'delay' and 'expire'.

        s :: N(dt non-negative) * (x * e) → x' * e  -- x delayed by dt
        t :: N(dt non-negative) * ([a→b]*e) → [a→b]' * e -- introduce expiration

Delay makes a good ambient authority because computing takes time. Here, expiration is a substructural type limits how much further a block can be delayed. It is an error delay a relevant block beyond its expiration, or to use an irrelevant block.

There may also be a synchronization operator of some sort. It seems feasible to leverage implicit synchronization, but it isn't clear whether this will result in a clear 'failure' location with regards to expiring blocks.

There doesn't seem to be a role in ABC for spatial structures, unless I create an ambient authority for an orthogonal pure-space or logical-space model. Of course, there is an issue here regarding spatial idempotence; I'll need extra arguments to enter 'distinct' logical spaces. One option is to use two operations:

        n :: (Location u) ⇒ u * (x@p * e) → u * (x@{u|p} * e)
        x :: (Location u) ⇒ u * (x@{u|p} * e) → u * (x@p * e)

I need a good notion of what logical location actually means, some useful properties.

* Concrete spaces (CPU, GPU, etc.) should be expressible as a disciplined/constrained application of this model, with some extra effects. I.e. this model should express a simplified representation of concrete space locations.

* It should also be feasible to maintain a separation of logical spaces across physical partitions. I.e. computing 'in parallel' relative to another location.

* There must be clear circumstances in the spatial model for which combining two values (e.g. adding two integers) is clearly a type error. It must be possible to engineer these circumstances to occur, and possible to control interaction between spaces.

* I'd like to model 'sealed values' this way, somehow. Or perhaps a weaker variation of sealed values? Something useful, anyway, perhaps enough to support ADTs.

Let's see if we can find a set of constraints towards a singular design!

**Constraint:** location should be deterministic.
**Constraint:** `nx` and `xn`, if safe, should be identity.

Between these two features, I must reject idempotence. Otherwise `hgg` and `hhg` might reduce in non-deterministic ways. So this leaves an option: I can either use type errors to reject `hh` and `gg`, or I can use a counting method that places computations in an infinite ad-hoc multi-dimensional grid. I somewhat favor the latter, since it is more symmetric with latency (relative, not absolute!)

**Constraint:** support generic programming.

If `hh` is a type error, then generic programming will be hindered because we cannot locally reason about logical locations. However, it seems acceptable to constrain dimensionality to a known non-negative value, i.e. if we have `g` we must also have an earlier `h`. 

**Soft Constraint:** can assert locations.

Restricting to non-negative locations enables useful assertions on location, i.e. `gh` would have meaning as a typeful assertion that we're in a given logical location. 

**Constraint:** logical location model is compatible with concrete locations
**Soft Constraint:** work well with quotations.

Either quotations `'` must capture objects all at one location, or they must be able to restore locations. While "restore location" seems okay for logical locations, it seems like a bad idea for concrete locations. So, quotation requires that all objects in a product have the same location.

**Constraint:** work well with literals.

???

**Soft Constraint:** control effects in logical space

Effects are generally limited in space, i.e. where are they available; where do they apply? 

**Soft Constraint:** (maybe) move concrete location, holding logical locations?

Actually, this might be best modeled by treating 'cross' as a polymorphic effect with regards to logical location.

In a sense, I don't want to explicitly restore relative locations when I cross physical boundaries. A logical space is parallel to physical spaces. Consequently, I must think in terms of a 'grid' rather than a 'tree' of spaces. 

*Thought:* It seems feasible to use a simpler type model. Only 'effects' (capability invocations) are truly immobile. Maybe I only need to distinguish 'anywhere' effects from 'somewhere' effects. But... hmm. It seems I'll still need concrete locations for integers, etc..

Most of these spaces are more about bindings of effects. 


* It seems, if I'm going to model different locations, that I need some means to represent spaces with 'rules' to how we can progress through them. This suggests a richer model for locations than just text identifiers. How might such a model of spaces be expressed? A grammar?

Perhaps, when moving into a new logical space, we must also specify the rule for moving out of it? Ugh, no. The logic should be maintained elsewhere, using blocks and linear types. 

It seems an ambient authority for logical space should be okay; it's just a way of logically partitioning data so it can't accidentally be used in the wrong context. How can we do this better?


 Notion of setting rules or challenges for moving into a new space seems very interesting, but is perhaps better modeled by encapsulation within blocks. 

# Much Older Stuff

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

# Thoughts (2014-01-07)

Perhaps I could approach spatial coordinates in terms of 'visibility', similar to a (perhaps heirarchical) scoping model. 

It seems that any spatial model is going to centre around "when can I apply a block". Perhaps the answer will be that a block may only accept parameters 'visible' from the block's scope.

I still don't have a good answer for how literals will interact with temporal models, especially in context of 'expiration' for blocks. But the more I think about it, the more I believe that spatial-temporal types might be better treated as effects outside of ABC proper. 

In that case, I might model delays and expirations as effects involving logical clocks. I could do this in a simplified manner: a clock delays a value to the time on the clock, if it is not already so delayed... and an expiration-clock is a specialized clock for operations on blocks (or values in general, wouldn't hurt). This way, I don't need to assign a time to literals, beyond the block or stream they are part of. 

If time is modeled effectfully, then SO SHOULD BE SPACE, for symmetry reasons. This also has some nice characteristics - notably, that I'm not tying myself to any particular spatial model. 

... okay! yay! I think it's settled now. Let's wait and see if it stays settled after bootstrap.
