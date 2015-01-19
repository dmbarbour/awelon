

Tail call optimization allows application of a block `[e→e']` without growing the stack. One tail call opportunity that is trivial to recognize at the bytecode layer is use of `$c`, which is frequently part of the inline operation `vr$c`. 

A possibility exists to recognize tail calls on sum types. For example, a tail call on a pair:

        (L+R) * ( [L→a] * ([R→b] * 1)))

            %w?VRWw?WLCc

        (a+b)

There is a tail-call opportunity here. In addition, there are variations where we may need to merge results as part of the tail call. I think pursuing tail-calls on conditional operations could be a big benefit, long term. But it might require recognizing a lot more opportunities.

Meanwhile, for a lot of cases, we might merge the results.


        (L+R) * ( [L→a] * ([R→a] * 1)))

            %w?VRWw?WLCMc

        a

This could be re-expressed in terms of binding the left and right values such that we have `[1→a]` then apply it.




