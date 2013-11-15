#### (Thought:) Promise Pipelining and Lattices

My intuition is that incremental process model could be greatly augmented with [promise pipelining](http://en.wikipedia.org/wiki/Futures_and_promises#Promise_pipelining). The process could actually run a few steps in advance of the effects, computing the dataflow before the data is fully computed. 

The motivation for this would be to tighten up latency and well-timing properties, and perhaps gain some efficiency via batching.

Interestingly, we could also use lattice based promises, such that we can observe an 'incremental' (but not necessarily 'final') value. This technique could result in an imperative system that is very robust even when timing falls slightly behind. (cf. Lindsey Kuper's [LVars](http://lambda-the-ultimate.org/node/4823))

