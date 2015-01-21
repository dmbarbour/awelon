
I'd love to perform more partial-evaluation optimizations on the linear forms of code.

This is trivial for cases such as:

        #123 #456 w  can be rewritten to   
        #456 #123

But it seems a bit less obvious when working with unit values, the AO stack, hand, and so on. In that case, we may have something like:

        123 456 swap take

The set of rewrite rules isn't nearly as clear, and may need to be very sophisticated. It may be necessary to develop a more generic approach to partial evaluations involving these structures. Though, it may still be useful to treat this as operating on a relatively static underlying environment. I'd really like to handle basic partial evaluations easily.

A generic approach might involve computing a value, tracking the unknown dynamic arguments. Unfortunately, this may require recomputing code in an ad-hoc manner. OTOH, that is not (or should not be) a major challenge.
