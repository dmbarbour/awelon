
## Ambiguity and Program Search (Experimental)

Ambiguity is syntactically expressed by wrapping a section in parentheses, and separating one or more options with a vertical bar. For example:

        a (b | c d) e (f|g|h)

The meaning of the above subprogram may be any one of:

        a b e f
        a b e g
        a b e h
        a c d e f
        a c d e g
        a c d e h

The choice of meanings in a given use case is left to the AO compiler. Formally, the choice is non-deterministic, but it is not random. Similar to ambiguity in natural language, ambiguity in AO is *resolved in context*.  Choices are eliminated if obviously not typesafe in context. 

The remaining choices may be searched based on a heuristic functions, which may evaluate options for performance, size, confidence of safety, stability (across versions of a program), and programmer annotations. Through annotations and control of the heuristic function, programmers can effectively influence the compiler's choice, leading to a good solution. 

There is never a guarantee that an optimum (or even optimal) solution will be discovered. It is not difficult to express programs with a hundred options for 2^100 meanings or more. With such large search spaces, non-exhaustive mechanisms must be used to select a 'good' program - e.g. hill climbing or genetic programming. 

The choice operator `|` is *commutative, associative, and idempotent*. The syntactic order in which choices are expressed must not contribute to heuristic evaluation of choices. This independence is important for refactoring ambiguous programs, and for optimizing search. It also means that, formally, we can understand ambiguity as expressing a *set* of programs. 

Ambiguous code has many roles: rapid prototyping, self-healing or adaptive code, exploring design spaces and tradeoffs, and tactical theorem proving. Adding or removing ambiguity allows programmers to shift gradually between 'crystalline' code with rigid structure and 'fluid' code that can reshape itself for its context. Developers can control where and how much adaptation occurs.

### Constraining Ambiguity

Context-dependent or non-deterministic meaning can be semantically troubling. For example, it hinders equational reasoning. Also, search is expensive. Ambiguity should be avoided or removed from a codebase when there is no obvious benefit from it. Where feasible, we should push program search to edit-time, and perhaps resolve ambiguous code at edit-time. 

A good programming environment can help developers manage ambiguity:

1. ambiguous words are styled or colored differently when rendered
2. automatic tests may be set to fail if specific code is ambiguous

The first technique helps developers recognize ambiguity without digging deeply through code. The second technique helps control ambiguity, preventing it from stealthily entering the dictionary.
