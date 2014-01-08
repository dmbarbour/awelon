(Extracted from section on Unique Types in AboutABC)

* a conceptually unique UID

By 'conceptually unique' I mean that the type system and developers can treat it as unique with respect to most reasoning purposes. A conceptually unique UID might only be cryptographically unique when serialized, i.e. using some construction like `"uniqueTextGoesHere\n~[{asUID}]$`. Valid operations on a UID include copy, drop, quotation, and equality comparison. 


Why not UIDs?
=============

The main issue I have with UIDs is the restriction to equality comparisons. 

With this restriction, I must either provide a primitive `=` operator... or ensure a stable comparison between UIDs, which means I'll be computing their value anyway. The latter would make the UID concept quite pointless from a formal reasoning standpoint.

I'm reluctant to provide the `=` operator for UIDs essentially because `=` based search is limited to linear structure, and I can't be smarter about it without losing determinism (in this case, deterministic orderings when serializing structure). 

With `>` I make `=` the path of greater resistance and push developers towards linear sorted structures. This is useful in context of a smart compiler because I can deterministically recover a linear sorted structure from a 'smart' native structure; alternatively, I might adjust code to directly model 'smart' sorted structures, and inject sequencing where needed. 

LSTs - linear sorted structures... and compiler awareness. Could be a powerful combo. By comparison, UIDs - which are really only usable as keys - seem much less useful.
