Users will gain an intuition for what it means to manipulate objects in their local spaces. It seems to me that this intuition should also, in many cases, be applied to objects in shared spaces. 

Essentially, we'll need some model for using ABC code to manipulate an 'external' object in the same manner that we manipulate those internal to our personal spaces.

I see two difficulties with this:

* Manipulations of local objects are currently exclusive.
* I would prefer to have only one manipulation model.

To resolve this issue, there are at least a few possibilities:

1. manipulate personal objects as though controlled by an RDP system
2. include special support for 'streaming' tacit concatenative operations to a shared space
3. take ownership of a shared-space object, manipulate it locally, then put it back

Of these, the first option is perhaps my favorite. The personal space is modeled as:

* user is a source of continuous signals (location, gesture, video, intentions, etc.)
* declarative state manipulations even in the personal space
* some mechanism for interpretive manipulations, e.g. animated reactive term rewriting

In this design, ABC code may still be useful for modeling the state updates and representing 'functions', and perhaps even keeping some history. However, the user is not directly streaming ABC code. There is (1) a layer of indirection, (2) a fixpoint requirement for all 'instantaneous' manipulations.

It is not clear to me that this model is realistic. Can I have animated reactive term rewriting? How much can I scale if I have any non-local effects? I might need a dedicated state model anyway, for the user environment, in which case it might be a poor fit for a shared space unless, by nature, it supports multiple agents. 

If the first option doesn't work out, the third possibility seems infeasible to me. It would require a notion of 'moving' state between spaces, and the extra step would be a specialization for the external space.

The second option, streaming to a shared space, is unreasonable because streaming code is exclusive. However, it might work out if hybridized with the first point, modeling this as pushing a block of code as a signal to a term rewrite model. The layer of indirection might work out.

**Manipulations within must be the same as manipulations without.** Not for ALL models of state, but at least for a useful subset with which humans directly interact. It should be possible to manipulate this same state from within the RDP model. I.e. using term rewriting.

Ultimately, I plan to maintain the "state is meaningful" feature, the design principle that all long-running behavior is represented in RDP state. But if I can get more uniformity on short-running state manipulations, I think it would improve the lives of my users.
 
