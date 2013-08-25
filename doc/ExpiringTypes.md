### Expiring and Ripening Blocks

With dynamic behaviors and capabilities, it is often useful to control *when* a behavior is invoked - i.e. no earlier than, no later than. In RDP, these expressions are in terms of relative latency, and enable strong latency properties to be enforced - which can be useful for safety reasons, or garbage collection. 

        expires :: (Number * x) ~> x % for some types x
        ripens  :: (Number * x) ~> x % for some types x

These type tags can be applied to blocks. Since most blocks are static, the number refers to the latency requirement for ???.

I'll need to decide exactly how they're applied. Maybe it doesn't make sense to use them for static blocks, since those don't have latency. Or maybe I need a reference latency for relative operations, something like: `(Number * (Block * Ref))`.

