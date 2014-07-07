
## Ambiguous Awelon Bytecode (AMBC)

Ambiguous ABC (AMBC), is an extension to ABC to directly express AO's ambiguity feature. AMBC serves primarily as an intermediate language for ambiguous AO. *Note:* Since ambiguous AO might be deprecated in favor of pushing program search to the IDE layer, AMBC might never be used.

AMBC extends ABC with with `(|)` characters, which represents a choice of subprograms separated by the `|` bar. E.g. `vr(wl|>M)c` has two potential meanings - `vrwlc` (swap) or `vr>Mc` (sort2). The resolution of this ambiguous meaning is not deterministic, but is constrained by typeful context (i.e. meanings with obvious type errors or assertion failures should be eliminated) and may further be guided by heuristics (i.e. weighted preferences or probabilities via annotations). 

Ambiguity is a potential asset for rapid prototyping, exploratory programming, adaptive code, and incremental refinement. Developers can represent a very large *space* of programs or data structures in a small volume of code, and may further implicitly constrain this space by use of types and assertions. This space can feasibly be explored by a number of mechanisms - i.e. satisfiability solvers, genetic programming, iterative hill climbing. 

There is a tradeoff. Resolution is expensive. It is difficult to correctly resolve effectful meanings in a streaming context. Equational reasoning, and reasoning in general, are hindered.

These weaknesses can be mitigated. In a typical AO context, resolution is at compile-time, and thus search can become a dialog with the developer. A good IDE can expose active choices to the developer and encourage refactoring of stable subprograms into non-ambiguous components. Average expense could be further reduced by application of machine learning to efficiently identify good meanings in context. In a streaming context, one might resolve for a group of paragraphs at a time, or favor an effects model that delays commitment.

AMBC can be used together with ABCD or `{#secureHash}` sources.

