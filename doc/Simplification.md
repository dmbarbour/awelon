
I'd love to perform more partial-evaluation optimizations on the linear forms of code.

This is trivial for cases such as:

        #123#456w  can be switched to   
        #456#123

But it seems a bit less obvious when working with the AO stack, hand, and so on. I might need a bunch or more sophisticated rules, or a more generic approach to partial evaluation and regenerating a structure around parts of an unknown dynamic input.



