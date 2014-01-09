(Extracted from section on Unique Types in AboutABC)

**Attempt 1:**

* a conceptually unique UID

By 'conceptually unique' I mean that the type system and developers can treat it as unique with respect to most reasoning purposes. A conceptually unique UID might only be cryptographically unique when serialized, i.e. using some construction like `"uniqueTextGoesHere\n~[{asUID}]$`. Valid operations on a UID include copy, drop, quotation, and equality comparison. 

**Attempt 2:**

* a conceptually and cryptographically unique identifier (UID) 

A UID is essentially a large, securely random string such that the global probability of collision is negligible - e.g. encoding 192 bits. However, the value comes wrapped in an opaque UID type, such that the type system may be aware of its unforgeable, unique status.


Why not UIDs?
=============

The main issue I have with UIDs is the restriction to equality comparisons. 

Without this restriction, I must be deeply concerned with stability of UID comparisons, and the UIDs will carry information beyond identity (introducing risk of covert channels), and ordering is effectively non-deterministic (as far as the subprogram using the UIDs is concerned) which introduces its own challenges for testing and reasoning.

With the equality restriction, I must provide a primitive `=` operator and encourage linear-search behaviors. In this case, the ordering in a list will be 'deterministic', but will not be stable to reorganization of the application software (i.e. because insert order cannot commute). Further, a 'sufficiently smart compiler' cannot replace the linear search structure with a smart alternative because I end up preserving ordering information that isn't essential.

If I can recognize sorted inserts and removals, and otherwise don't operate on the list, I presumably could recognize this pattern and replace it with a hashtable or similar. When necessary, I could recover the original sorted form. Ultimately it's taking advantage of simple, formal concepts like "information about insert order is lost". In lieu of a 'smart' compiler, this would be feasible with a 'dumb' compiler plus a few annotations.

Linear sorted structures and compiler awareness... could be a powerful combo. By comparison, UIDs - which are really only usable as keys - seem much less useful in the broad range of contexts.
