I'm interested in the possibility of modeling algebraic effects in Awelon. 

To make it convenient, I'd probably need something like a pair of operators:

        y :: (a*e)→(b*e) (yield-with-argument)
        Y :: ([x→z]*(x*e))→(([b→z]*a)+z)*e  (call-with-yield)

The hypothetical `Y` operator can trivially implement call/cc and other effects. The toplevel yield, together with sealers, could also replace the common powerblock. The original `call` operator, $, would still exist so we can use blocks for pure abstraction (whereas Y uses blocks as effect frames) - i.e. `$` and `YK` have the same type but are not the same behavior.

The advantage of such a design is that it allows me to easily reify the notion of an 'execution environment' within the language. The disadvantage is that I cannot readily reconcile use of 'yield' with causal commutativity or spatial idempotence - not without requiring a lot of discipline with respect to modeling execution environments. The 'environment' is *implicitly* serialized by the use of 'yield'. 

And this seems to be an essential tradeoff for any schema... causal commutativity and spatial idempotence require careful design of the environment, which contraindicates features supporting implicit reification of a computation environment. And such schema also require a lot of implicit reasoning about the environment. In context of Awelon, I believe the disadvantages outweigh the advantages. 

Is there a variation we could try? Perhaps I need some model of concurrent capture to make this work... but I'm not sure how to achieve such a feature. 

Ultimately, I suspect algebraic effects won't be very convenient to express in Awelon. 

* We could model them in a higher level language then translate down to ABC, but that would require some whole program transforms.
* We could perhaps model partitioning and concurrent effect capture by use of capabilities, thus controlling the integration with the environment. This approach is more promising, but won't be very portable (unless specified as a standard for certain distributed runtimes). 
* Explicit, incremental process models - e.g. of form `µP.[a→(P*b)]` for some dependent set of a,b pairs, which already matches the typical powerblock type - are also reasonably close to expressing algebraic effects; the `b` type can represent commands and other outputs, and `a` can model inputs and responses.

