Negative and fractional types, i.e. as developed by Amr Sabry [(1)](http://www.cs.indiana.edu/~sabry/papers/rational.pdf), are an idea I find fascinating. I've been tempted many times to include primitive support for these concepts in ABC, but each time I eventually decide against it. I'm documenting my thoughts here, mostly so I can review my reasoning more easily.

My main motivation is that fractional types represent a sort of 'promised value'. I.e. we can have a primitive function of type `1→(a * 1/a)`, representing a promise for a and a function to fulfill it. Actual fulfillment may then be represented by a function `(a * 1/a)→1`. The interesting property here is that a type `1/a` is implicitly pipelined from right to left through the datashuffling operations.

The difficulty with fractional types seems to arise from a few points.

1. difficulty of validating them for safety. Even a simple model can become divergent if a promise is somehow (even indirectly) used to fulfill itself. It is difficult to analyze for this possibility at the local scale.

2. presence of a spatial/temporal model. Having fractional types implicitly flowing 'upstream' seems to require uniformly bidirectional dataflows through space-time. But I believe there are big advantages to constraining dataflow in both space and time, i.e. of having many paths be irreversible (and effectful).

3. much more difficult to reason *structurally* about causal commutativity or spatial idempotence if we can have inputs from the right-hand side. I.e. I need to know a lot more about the specific types involved, rather than the generic shape of the program.

Instead of fractional types, I could model promises well enough by use of effects and substructural types. I.e. I can 'create' a new promise, if I must. But the reduced convenience - i.e. this requires some uniqueness model - is likely to undermine the utility. OTOH, that might not be a bad thing; in some ways, promises are already diminished in utility because I do have causal commutativity and implicit parallelism.

