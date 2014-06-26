
Awelon Bytecode has always been intended for compilation, to support performance. Currently, this is performed entirely at the bytecode layer, but it might be worth while to compile at the AO layer as well.


## Controlling Compilation

However, a reasonable question is just how I want to go about achieving this. 

One approach I've tried is to compile the full dictionary into a Haskell file, operating at the level of individual words. This achieves decent performance, and could likely do better (my original compile was quite naive). But it also requires a big, expensive recompile after any change in the dictionary. I did not like this consequence, and eventually scrapped the approach.

Another approach I've developed is use of dynamic compilation, a `{&compile}` annotation that operates on a block. This is potentially very useful for dynamic code, but at runtime it can lead to large overheads. Potentially, we can also perform JIT based on recognizing how often a block is called.

An interesting possibility that I have not developed: precompile just a subset of words, and load them as plugins. These words might be identified by name, e.g. using prefix `#` such that `#foo` is automatically compiled into a `{#secureHashResource}`. This could feasibly simplify development of 'hard' vs. 'soft' layers, and could jumpstart support for secure hash resources on larger scales.

If I do this well, I shouldn't need to recompile the whole dictionary on each change... only a small subset that is both used and precompiled. This should support effective performance and perhaps better reuse of code and memory. 






