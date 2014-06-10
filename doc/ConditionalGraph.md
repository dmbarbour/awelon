I've been trying to develop a decent intermediate language for compiling ABC code into Haskell. A significant challenge has been the representation of conditional behavior.

If we have linear, non-conditional behavior, we end up with a dataflow graph that produces and combines numbers, and occasionally applies a block. In a sense, ALL the dataflows in this graph are 'active'. This is a rather nice feature, really.

The introduction of conditional behavior in the representation requires some careful consideration. 

Presumably, it allows that some dataflows are 'inactive'. In this sense, perhaps what I have is a set of `Maybe V` types, representing wires at runtime. This could be a useful view, perhaps, since I can easily define some conditional operations via MaybeT. This also corresponds more closely to how independent dataflows are represented in Sirea/RDP (albeit using signals with Maybe values).

In this case, with a sum type `(a+b)`, the representation in Haskell will be `Maybe a` and `Maybe b`. And for composition's sake, I might also model arbitrary wires with the appropriate operations.




