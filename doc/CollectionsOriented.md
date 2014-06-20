
I am fond of the idea of collections oriented programming - i.e. where arrays, matrices, sets, tables, or databases are the primary value types and most primitive operators are designed around them. Languages with this orientation include APL, J, K, Bloom, and others.

A major advantage of collections-oriented programming is that collection processing operators can be highly specialized, efficient, and readily optimized at a higher level. Presumably we can get similar benefits from rewrites like `[foo] map.l [bar] map.l = [foo bar] map.l` where `map.l` is a list map operation (this holds even for effectful ABC due to causal commutativity). 

A corresponding disadvantage is that the collections models can be rigid and awkward for specific purposes.

As currently designed, Awelon Bytecode (ABC) does not have any primitive operators for collections processing. Primitive operators like `+` and `*` and function application operate on scalars. Combining two lists together generally requires a fixpoint operation (e.g. `[^'wo]wo^'wo`) to replicate the function. 

However, ABC does include provisional design for ABCD - Awelon Bytecode Deflated - that extends the basic set of operators with a dictionary of common, widely used functions. These functions don't need to be very primitive - they can potentially include highly specialized tasks such as converting text to UTF-8 binary, SHA3-384 secure hash, binary to Base-64, LZHAM compression, and so on... so long as those programs can be proven correct and unlikely to change. But most likely, ABCD will be used for more conventional tasks such as mapping a function across a list, managing association lists, working with matrices and vector math.

I hope that ABCD will eventually include a useful library of collections-oriented programming operators, such that we can get some advantages in performance and parsimony similar to K or APL without losing the advantages of a minimal core language. 
