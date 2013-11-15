A feature I'm interested in:

If I'm modeling a set, association list, table, etc. then it seems useful to assert that certain ordering properties are not relevant or should not be observable.

Currently, ABC can model this indirectly by trapping elements within a block/function. However, I cannot express this property using 'pairs' unless I have some model of order-independent pairs (a 2-set, and unions thereof?)

I'll need to think about this. A 2-set might be quite doable, but would add a lot of undesirable complexity to ABC unless I can do it right. Modeling sets within blocks, or as sorted lists, might be a better option. So might be simply analysis/annotation for order-independent behaviors.

A computational 'reality' is that order cannot be eliminated, because physical properties, how information is communicated or stored. Order can only be ignored. But somehow formalizing that ignorance, beyond causal commutativity, seems potentially useful for distributed operations and modeling commutative analysis of sets.

At the very least, I should track cases where order-independence can be asserted.






