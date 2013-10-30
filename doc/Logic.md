Thoughts:

## Logical AO?

In earlier designs of Awelon, I was considering a logic programming like feature that worked as follows:

* Each word may have multiple definitions.
* Thus, any sequence of words may have a large set of meanings.
* The implicit goal is to find (by search) a program that typechecks.

I decided against this because:

* ambiguity, non-deterministic meaning
* difficult to control performance; exponential easily
* difficult interaction with reactivity 
* difficult to type capabilities

But I still wonder whether the idea has some merit. In a sense, definitions become tactics for proving a theorem.

## Probabilistic Generative Grammars and Soft Constraint models

I'm very interested in generating structures that can be used based on given rules and constraints. The "Logical AO" above might be an interesting approach.




