Thoughts on Hutchin's [feature oriented programming](http://www.infosun.fim.uni-passau.de/cl/publications/docs/TOPLAS10.pdf) (FOP) with regards to Awelon project.

FOP describes features as refinements of a base program. Before reading the article, I'd say that Awelon's approach to this involves:

* distribution of capabilities, which can enable deep control over effects invoked by a subprogram (and extension), especially due to externalizing state
* potential models for aspect-oriented designs, based on having some functions search for and install code from the named stacks.

Sven Apel and Delesley Hutchins present gDeep, a calculus for function composition. In this design:

* a base program is modeled as a collection of named program elements (yuck)
* which are organized in a hierarchical namespace (double yuck)
* and a feature, a 'program refinement' is effectively a patch on this space

The approach of using patches based on named spaces strikes me as both fragile (e.g. to different permutations, combinations, renaming) and quite difficult to reuse. We shouldn't be naming components, but perhaps naming the roles those components play.

I suppose I do have named hierarchical structure in Awelon project, just in a different layer: I model forking the 'powerblock' in terms of parent/child relationships, where each child is given a text name. The powerblock doesn't reuse child names. There may also be role names, when forking, to help determine default security policies. 

The ability to refine a program "at any depth in the hierarchy" is quite feasible (and sensible) with regards to refinement of capabilities. And I suppose the powerbox would also be a good vector for registering refinements/extensions and feature-sets for voluntary use deep in the program (i.e. an extra repository of data, apart from named stacks). 

But I don't offer an ability to refine 'documentation', at least in the absence of self-explaining code models.

One thing I would like is a solution to the expression problem... some model of typeclasses that can be extended and threaded. Use of the powerblock as a means to thread many typeclasses (and acquire them externally) seems promising. I could also do so via the larger environment, but a powerblock might be a more reliable delivery mechanism.

I would also like to model mixin/traits-like composition at some point (where traits applied to an object are commutative and associative). I think this shouldn't be difficult to model directly in AO. It may require modeling objects or modules within the value structure of the language.





