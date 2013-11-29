
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

