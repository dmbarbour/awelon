This document regards potential process models for the `ao exec` and `aoi` utilities (which are functional/imperative in the toplevel). 

## Design Constraints and Concerns

AO must respect causal commutativity, spatial idempotence, and progress. Optimizers are allowed to assume these features are enforced by the effects model. Other desiderata include continuity with RDP and Awelon project, simplicity and security, determinism by default (indeterminism is explicit), and temporal commutativity with respect to operations on independent resources. And performance, of course!

The conventional wait-do-loop imperative process model would violate most of AO's assumed properties.

### Continuity with RDP and Awelon Project

To support continuity with RDP and Awelon project, a few considerations:

* In RDP, regarding the resource model:
    * All resources are named and external. Discover, don't create.
    * Some resources are abundant - as many as names.
    * State resources are persistent unless the model implies otherwise.
    * Should consider modeling spatial partitioning of resources.

* In Awelon project, regarding the process model:
    * The toplevel stream manipulates a data structure.
    * The data structure is interpreted as a live RDP program.
    * All long-running behavior is staged into the RDP layer.
    * Process control is by manipulating live programs.
    * Capabilities are distributed into subprograms via the toplevel.
    * Child capabilities can be constrained by parent.
    * Constraint of child capability is commutative with distribution.

In Awelon project, we have an interesting staging concept for effects. Capabilities are often distributed in one stage, then invoked in another. This separation enables users to control distribution of authority (no 'implicit' capability for child processes). Inherently, the toplevel stream is also considered a process, taking place over time. 

It might be a good idea to skip this 'imperative AO' concept and simply implement Awelon an project based environment directly for `ao` and `aoi`. Though, it might also be useful to explicitly 'commit' the updated program when necessary. 

### Toplevel Stream

Both `aoi` and `ao exec.s` allow processing of an indefinite AO command stream as a sort of 'toplevel' process. In some senses, this is a long-running behavior. However, for simplicity of reasoning and refactoring:

* any sequence of commands or paragraphs should equivalent to their composition

This suggests that effects in the toplevel stream should not be time-sensitive. Or, to the extent they are time-sensitive, we'll also have some non-determinism. Anyhow, this also means that causal commutativity and progress require some careful considerations:

* progress: an observation cannot wait for the unbounded future of the stream
* causal commutativity: wait on concurrent influences on state before observing

Thus, we cannot model concurrent manipulation of interacting resources in the toplevel stream. For example, we cannot send a message from one subprogram and read it in another... that would violate either progress (waiting on the message) or commutativity (reading 'no message available' vs. 'message available' depending on whether another computation runs first). However, we may model concurrent manipulation of truly independent resources.

At this toplevel, what sort of effects should we expect to observe, anyway?

* snapshots and recordings for state and sensors
* process and state control (pause, reset, swap, etc.)

Even in Awelon project, I would expect some snapshots of sorts in certain cases of information sharing between users. It's a way to 'lift' information back up a few stages, e.g. pulling it out of a mailbox. A snapshot is motion of values between stateful resources. OTOH... it might not be necessary to model these effects using a powerblock at all. Snapshots and resets could be lifted into the user's programming environment, rather than internal to it. 

Perhaps I should pursue that direction of shifting snapshot effects outside the toplevel environment, so long as I can still abstract some useful behaviors. 

## Current Direction

Right now, I'm leaning more towards implementing the Awelon project model straight out. This would get me to where I wish to be with fewer intermediate steps, and perhaps less risk of getting 'stuck' in an intermediate step as a local minima. The cost is that I'll need to develop the RDP implementation of ABC earlier in Haskell. OTOH, that isn't a terrible cost.

Previously, I've contemplated use of incremental processes (`µP.[a→(P*b)]`) several times, but those create all sorts of painful challenges regarding process control and live update. Actually, I don't know of any imperative process model that is amenable to live programming and process control, except perhaps: `[(i*s)→(o*s)]`, with `s` looping as persistent state from step to step. (Perhaps I could support this model in some cases, assuming I can figure out how to initialize it.)

For now, I'll continue with the top-level 'powerblock' for the stream to help guide interpretation, installation, snapshots. Actually, it might serve better than direct 'interpretation' of state, to explicitly commit and install the reactive programs. The important part will be preserving the values so committed in their pre-interpreted states, such that I'm free to 'commit' them again and again (with slight modifications). 

## Toplevel Powers

TODO: figure out what the AORT toplevel powers should be, and how they interact with RDP powers.

Thoughts:

* introspection of a block; see what's inside any block
* confine evaluation of a block; control propagation of error
* obtain snapshots of state resources 
* live-update program or state for some resources

* install RDP behaviors for web application services
* obtain source-stable unique values
* obtain 'clocks' used for delay in RDP
* constrain authorities granted to 'child' RDP powerblocks?
    * this might be better as RDP-only; constraint is reactive
    * important feature: constraint is commutative with forking
* adopting of child powers to limit constraints
    * again, reactive...
* persistent state resources

Interestingly, we don't need access to the dictionary or files to support bootstrapping. Introspection of a block will be sufficient, together with fixpoint patterns. Still, the following might be a little useful:

* access dictionary as a special resource?
* access files as resources?
* Haskell code or plugins support?

I'd rather eliminate dependency on these entirely, though. We should model the AO dictionary within the AO user environment. We should model files within the AO dictionary. So, I'll put these features off for a while. 


