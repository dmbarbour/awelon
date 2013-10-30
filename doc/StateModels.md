Constraints for Awelon Project:

* External state are volatile - existing for publish, subscribe, discovery.
*   Combined with RDP, we don't need global GC.
*   External state resources may keep simple, logarithmic history models.
* Persistent state resources are constructed within apps, and have identity.
*   Acquired at unique location in program (use uniqueness source)
*   Point of acquisition also specifies state model.
*   Models have names and versions. 
*   State model & existing state can be updated. Rule is contained in model.
* State models must handle a "no input" possibility. 
* Support multiple observers, enabling extension, auditing, debugging.
* Support for single-writer state, or master/slave writer patterns?
*   Writer capabilities depend on the state model. 
* State resources may have a single writer, to simplify the update model.

Awelon/RDP is constrained by 'declarative' state models. State at any given instant is influenced by a set of zero or more signals. The value for simple state is a fixpoint function of the current set of signals and its previous state.

A goal will be to support compositional/decompositional state, such that I can easily distribute or combine parts of a state model without changing the meaning.

## Simple History Models

A few simple, broadly useful state models would be 'demand monitor' (a zero-history), 'windowed history' and 'exponential decay of history'.

## Tuple Spaces

Tuple spaces could be a very effective state model for RDP systems, with or without expirations. If tuple models are built into a compiler, it might even be possible to optimize transport so state doesn't flow through an intermediate storage.

## Stateless Stability Models

Signals represent constraints or similar, and the goal is to find solutions that remain stable over time (in actual practice, to account for loops). Machine learning might be useful in this context. Stateless stable models should be useful for planning, artistic work, control models.

## Term Rewriting

A potential state model - one I'm reluctant to use due to non-determinism - is reactive term rewriting, where each writer may provide a function that attempts to rewrite the state (potentially failing). An interesting feature is that many rewrite rules can be built into the model, and the state can potentially contain functions to apply later.

Interestingly, Awelon project might make better use of this by using a linear writer capability that must be explicitly forked, such that each writer has a unique id and any ordering based non-determinism can be eliminated.

The main difficulty with term rewriting is that there is no assurance that it will converge.

## Animated State?

A possibility exists for 'animated' state, where the state of the model depends on when the signals are processed, and the state model can effectively change even without observation or influence. In general, animated state must enable efficient computation of the current state up to a given point in time. 

Sadly, I currently lack a simple theory that covers all animated state. The issue is dealing with updates to influence signals while the new state is logically being computed. 

Thought: provide a linear capability for at least one of

* generating the current time plus some increment
* comparing with the current time

The idea here would be to use the lower of these values to decide when to cycle and compute again, yet also constrain operations on time well enough that I can detect cycles at the state layer itself, and thus reduce long cyclic animations to a function of the current time. (In particular, it is essential that state never depend on a concrete time.)

## State Machines? or Grammars?

I wish to avoid reactive state transition" or more typical state machines. State machines have terrible composition, extension, and scalability properties. Grammars can serve the same purpose, but are more open to composition, extension, etc. because grammars name protocols/behaviors (instead of naming states). 

I plan to explore grammar-based state models for reactive systems. I am particularly interested in compositional models, so some support for spatial-temporal and concurrent grammars (i.e. with clear distinction may be interesting. It may be useful to use grammars to automatically "lift" sequences, building higher level detected event streams.

(Perhaps this would integrate well with a history model?)

TODO: explore grammar models for reactive systems. 

THOUGHTS: The 'state machine' concept of transition, in a continuous reactive model, becomes an edge with a set of transition signals on it (a logical predicate, or constraint). What would this look like if adapted to a grammar? I might need a concept of spatial-temporal grammars.


