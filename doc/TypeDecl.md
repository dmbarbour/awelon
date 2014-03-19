Currently, ABC has no effective way to declare types.

It used to be, you could use observer operators (`P`, `S`, `B`, `N`) together with assertions to say a little something about types. However, I've removed these observers.

An interesting possibility I'm considering is to introduce a concept of a virtual fork:

        X :: a*e → ((a+a)*e)  

Where, in this case, the result is certainly in the right-hand side, but (unlike `V`) we have some assumed type relationship to the left-hand side. The result could be merged with `K` or `M` (but not `C`).
