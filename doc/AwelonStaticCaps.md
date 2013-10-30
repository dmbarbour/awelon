
Awelon environment has two 'stages' of execution: build time and runtime.

A specific design goal is that all long-running behaviors represented in the runtime, such that they are always visible, accessible to the user for maintenance and process control. 

However, there may still be a role for 'short-running' capabilities at build time, i.e. that operate like pure functions or procedures. I can think of at least two:

**uniqueness sources** should be available at build-time in order to create objects that can be moved in the environment without destabilizing any binding to their external state. 

**block -> ABC code** translators should be available at build-time so users can inspect code received from others. (Further code, not a capability, might disassemble ABC code relative to an AO dictionary, or into some other language.)

Under consideration:

**sealer/unsealer pairs** would make an interesting build-time type for modeling ADTs and similar. If I have these, I might also want some model of **skeleton keys** to peek inside. (This could be a hierarchical model, only capable of accessing keys constructed on a particular path of the build-time powerbox.)


## Uniqueness and Power?

The motivation for distributing uniqueness at build-time is so that developers can build objects that can be moved in the environment without changing their access to state. However, I have a second goal, which is to keep structure of power visible. 

A question I have is: how should I integrate uniqueness with power?

If an object has a unique identity, it might be useful to treat this as an ability to 'unlock' runtime powers regardless of where they are in the environment. In this case, the power available to that identity is distinct from the powerblock granted to the identity. This might be an exchange:

* input is a generic powerblock (?)
* output is an identified powerblock

However, I also wish to control authority granted to 'children' along the unique identity path. Perhaps this technique will work: Instead of having a 'centralized' switchboard for power, the 'identified' powerblock is free to restrict powerblocks for ALL children, including BOTH those identities that have already been distributed AND those identities that have yet to be created. 

Basically, remove the ordering constraint.

Hmm. Restricting the child is ALWAYS an operation in the parent, and the order doesn't matter much. It might be useful to tweak the naming so that pre-distributed names are distinct from runtime names. But this could be left to convention, with the default being a smooth transition between compile-time and runtime objects.

I like! So let's go with this for now:

* restriction of child capabilities is always expressed as a runtime behavior
* restriction is not order-dependent, even if child id is distributed at compile-time
* uniqueness sources are available at compile-time, and must be lifted into runtime

Potentially, I could avoid the separate notion of a runtime 'powerblock' by having a more generic 'delimiter', or an opaque runtime-located unit value to translate compile-time powerblocks into runtime powerblocks. I'm not sure how I want to do this. Thoughts:

* if it involves passing the pre-distributed powerblock into another block, this might give too much power to the environment. I.e. it would be easy to sneak in auditors and transformers for an object's behavior that cause it to behave differently based on where it is located.
* OTOH, if it involves passing an opaque value to the powerblock, to unlock access to the more interesting runtime capabilities, then the power is controlled by the recipient.

In this case, I imagine I should use the latter design. We don't want the client program to give up its own embedded capabilities to the caller.

## Identity

When a parent splits into children, it must name each of them. A possibility is that, in addition to the programmer's text, we track a count of the number of times that name has been used (e.g. "bob" 1, "bob" 2, ...). This perhaps provides a balance between source-stable uniqueness and laziness. In addition, any policies on distribution of authority for "bob" can be repeated. 

Even better might be an option: users can ask for a multi-bob or just a plain bob, each time they fork. There may be extra constraints on authorities granted to a reusable path?

## Adoption or Ascendance?

An interesting case: a user wants to separate a particular object so it is no longer subject to the restrictions of its parent. A possibility here is 'adoption' - a capability that might move an object (and all embedded state, as-is) from downlevel to uplevel. Or perhaps 'adoption' is not the best word for it. Perhaps 'ascend'. This might need to be performed by the uplevel static capability (it's effectively a procedure), and apply only to a static cap.

## Runtime Capabilities are Distributed at Runtime

If runtime powers were distributed at compile-time, it would be difficult modify that distribution later. Instead, runtime powers are distributed at runtime.


If power is distributed via the static environment, the distribution process is lost to history - i.e. it is not visible, not accessible for editing or update. OTOH, if power of a mobile object depends on where it is installed, then it cannot readily be used as a capability, and perhaps depends too much on context. 

I could accept that users cannot easily manipulate powers accessible to mobile objects. (Don't like.)

Potentially, a location in the environment describes constraints for powerblocks distributed at compile-time, like a switchboard. This seems very divorced, but it might work well enough due to hierarchy.









Distribution of power to a mobile structure must still be accessible for editing. 







