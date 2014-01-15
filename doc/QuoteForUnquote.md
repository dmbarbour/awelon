At the time of this writing, the type for quotation is:

        ' :: x * e → [1→x]*e

I could potentially simplify use of quotation some by modifying the definition a bit:

        ' :: x * e → [s→(x*s)] * e

In this sense, a quoted value would more like a literal if inlined (`vr$c`). It could be unquoted quite trivially by application to the current stack, and such unquoting would leave the value on the current stack. It may be much simpler to use for currying and other tasks, since I won't need to arrange unit when I know that a value is quoted.

This seems like it has a cost for reasoning about the quotation, but OTOH it might actually strengthen it because I'm not making any assumptions on the argument. (Regardless, I can get the old behavior by composing `[%c]` with the quotation.)

But maybe this cost is acceptable if it means simpler currying behaviors, etc.. In some ways, the choice of 'quote' is adding a lot of accidental complexity to some operations. 

TODO: contemplate quotation further, make a decision. 
Resolution: make the change.
