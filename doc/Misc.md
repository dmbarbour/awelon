
## Another Assertion Primitive?

I currently have operator `K :: ((a+b)*e) → (b*e)` for runtime assertions.

However, it might be also useful to have a polymorphic:

        assertEQ :: (a*(a*e))→(a*(a*e))

This would be excellent for testing, and (even better) for asserting trivial (structural) equivalence between blocks that cannot normally be observed. 

For now, it might be better to assume this as a standard annotation, e.g.

        {&≡} :: (a*(a*e))→(a*(a*e))




### Data is represented by Code

Data serialization in Awelon systems is modeled by a stream of ABC code that reconstructs the data. E.g. a pair `(42,108)` might be serialized to `#108#42lc`. In general, this is similar to the quotation operator. 

Representing data as code has nice advantages for simplicity, self-validation, optimization, reuse, and laziness or procedural generation. Addending such code is equivalent to editing or transforming the value. Lenses and views are readily applied. 

For connectivity between Awelon project systems, data will uniformly be serialized in this manner. Additionally, a communication context - a value that belongs to the connection - will be maintained at each endpoint, enabling more optimal communication by storing some reusable macros or templates. 

### Capabilities Under-the-Hood

Capability text can be generated lazily, when the capability is serialized to the network. In practice, most capabilities will never be serialized, and thus the capability text never needs be generated. Under the hood, capabilities can be opaque object pointers. 

### Executive Capabilities for Partial Failure

ABC does not have any operators to catch runtime errors. This must is fulfilled by use of 'executive' capabilities, which operate similar to `$` but also admit the possibility of runtime failure - similar to the following (preferably with secure tokens):

        {exec} :: [x→x']*(x*e) → (x+x')*e

Executives are secured to resist 'defensive programming' strategies in-the-small which should be left to higher architectural layers. Developers can control which subprograms might operate in degraded form. 

### Tail Call Elimination

ABC might be implemented on a stack machine to handle blocks. In that case, [tail call elimination](http://en.wikipedia.org/wiki/Tail_call) is recommended unless stack size is statically bounded. If an applied block ends in `$]` or `$c]`, this is a likely candidate for tail-call elimination. Similarly, ABC has something like an inlining operation: `vr$c`.

### Linear Implementation or Garbage Collection

In ABC, use of drop and copy operations (`%` and `^`) is explicit. We effectively have explicit memory management, but with pure, immutable values. This opens many choices for the implementation:

* linear, deep copy up front, no garbage
* alias on copy, track refcounts, copy on non-linear write
* immutable values in memory, implement ABC as pure functions

Some designs will be more or less efficient than others, or more or less amenable to various optimizations (laziness, interning, memoization, etc.). ABC doesn't specify any particular implementation, and it might be worthwhile for a compiler to experiment empirically and use different techniques in different regions of code. 

### List Terminals as Type Tags

Lists in ABC are generally modeled as ending with a number. However, rather than uniformly ending lists with number 0, I suggest ending lists with a number that hints at their type. Suggested conventions:

* number 0 for generic lists
* number 1 for numerical unit lists
* number 2 for for Huet zippers
* number 3 for text (built into ABC)
* number 4 for sets (order, duplication shouldn't matter)
* number 5 for association lists (list of key→value pairs)
* number 8 for binary (integers in range 0-255)

This convention is easy to track statically and may simplify optimization, visualization, model detection, and type analysis. 

A special case is list-like structures that are intended to have statically known size (tuples, vectors, stacks). These might be terminated using unit, which prevents recursive introspection.

### Sequence Interpretation

In addition to being streamable, a nice feature of a tacit concatenative bytecode is that pattern recognition (at low levels) is often trivial. An ABC stream interpreter might achieve great performance by simply recognizing common sequences of code and substituting a function compiled for that sequence. For example, we might recognize `vrwlc` and execute an optimized full-swap operation, rather than perform each ABC command in sequence. 

More generally, sequence interpretation might be applied to optimize:

* common list operations (map, fold, addend, reverse)
* common association list operations (extract, modify)
* common number operations (exponentiation, matrix multiplication)

The runtime overhead for sequence interpretation isn't very high; it can feasibly be modeled using a finite state machine or trie. An interpreter could have such a machine built-in for known common sequences. An interesting possibility is to dynamically modify this machine based on history for a given instance of the interpreter, leveraging JIT. 

Sequence interpretation is related to ABCD, which will extend ABC with a dictionary of common sequences based on analysis of many projects. However, sequence interpretation is local to the interpreter, and potentially specialized to a problem domain or installation.




### High Level Data Shuffling

Many common words involve moving values and structures around on the current stack or within the environment. For example, `take` and `put` move objects between stack and hand. `juggleK` and `rollK` rotate objects within the hand or current stack respectively. `:label load` and `:label store` will treat named stacks as a form of global memory. `:label goto` will swap the current stack with the named stack.

Similarly, I expect AO will heavily leverage Huet zipper-based abstractions, which are essentially a form of data shuffling.

Shuffling operations are very first order. However, procedures can be built above them, and they often fade into the background. When coupled with iteration and search, shuffling can offer powerful transformations on the environment.

A single stack is good for a single task. Navigation between stacks is useful when modeling multiple tasks, concurrent workflows that are loosely coupled but must occasionally interact.

### Automatic Visualization

Programmers often reject concatenative languages without even attempting to learn them. My hypothesis is that the learning curve has been too steep: there is a burden to visualize the environment in the head, and a burden to learn a bunch of arcane shuffle words. 

If so, automatic visualization of environment structure, and animation of how it changes across AO code, should relieve much of this burden. 

Conversely, ability to use a little drag and drop or copy-paste through the environment visualization could help specify data shuffling in the tricky cases and when first getting used to the concept. It would be a weak form of programming by example.

### Document-like Structures, Zippers and Folds

When developing abstractions, it is best to favor those that are compositional, extensible, scalable, reusable across projects and problems, and for which rich domain-generic vocabularies and analyses can be developed. Document-like abstractions -  - tend to be much more composable, extensible, and scalable than, for example, records and state machines. (In some cases, they are also more general. E.g. grammars can model state machines, and tables can model records.)

A convention I wish to push for AO is to strongly favor these document-like extensible, compositional, reusable abstractions as the foundation for communicating rich structures between higher level components, even when it means rejecting problem-specific or domain-specific structure that might initially seem more convenient. 

Hypotheses:

1. reusable, composable abstractions will pay for themselves in the large, even if they initially seem slightly inconvenient in the small

2. manipulating document-like structures on the stacks will work well with standard visualizations, animations, and support effective intuitions and programming-by-example

An interesting feature of document-like structures is that they can be navigated and manipulated incrementally, using a purely functional cursor called a , a data structured developed by Huet in 1997. Thus, navigating and manipulating the environment extends to navigating and manipulating the individual documents. *Effectively, AO's programming environment is analogous to a desktop user environment, with different stacks representing different windows.* 

An interesting convention to help specify datatypes is to model folds: 


## Refactoring

There is significant utility of being able to refactor at the syntactic layer, i.e. by copy-and-paste or search-and-replace of common code segments. When refactoring is easy, it happens often and fluidly and is more readily aided by software. AO simplifies this further by favoring a flat namespace - there is no need to parse context to comprehend bindings.

An AO programming environment should help developers refactor, e.g. highlight sequences of five to twelve words that are seen many times in a codebase, or sequences that already have dedicated names. In the latter case, the AO programming environment would implicitly help developers discover and learn existing words. 

Beyond simple pattern matching, we must also concern ourselves with the boundaries, how definitions are aligned and coupled with words. For example:

        word1  word2            word3  word4  e
        a b c  d e      (vs)    a b    c d 

Well chosen boundaries results in more comprehensible, reusable code. AO words are at once functions, modules, and software components. Thus the boundaries correspond to interface and coupling decisions. Unfortunately, whether boundaries are well chosen is often unclear before there is a great deal of reuse in a broad variety of contexts. 

An AO programming environment should also support developers in exploring different arrangements of boundaries, in addition to other equivalent expressions of the same behavior based on blocks or causal commutativity.

## Dissassembly and Translation

An interesting property of AO code is that disassembly of a large ABC constructs is quite feasible in terms of matching code to a dictionary. Disassembly can be modeled as a parsing or refactoring problem. This feature could be used both for understanding ABC code, and for translating project code between highly divergent dictionaries.


