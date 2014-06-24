
Awelon Bytecode has always been intended for compilation, to support performance. Awelon Object language (AO) can also benefit from compilation.

However, a reasonable question is just how I want to go about achieving this. 

One approach I've tried is to compile the full dictionary into a Haskell file, operating at the level of individual words. This achieves decent performance, and could likely do better (my original compile was quite naive). But it also requires a big, expensive recompile after any change in the dictionary. I did not like this consequence, and eventually scrapped the approach.

Another approach I've developed is use of dynamic compilation, a `{&compile}` annotation that operates on a block. This is potentially very useful for dynamic code, but at runtime it can lead to large overheads. Potentially, we can also perform JIT based on recognizing how often a block is called.

An interesting possibility that I have not developed: precompile just a subset of words, and load them as plugins. These words might be identified by name, e.g. using suffix `.exe` or prefix `#` (it seems I favor the latter) to indicate that a word should be precompiled rather than interpreted. 

This approach could be very effective to support static compilation, and perhaps to support partial reuse (wherever one precompiled word invokes another). Indeed, precompiled words might be compiled into `{#secureHashResources}` at the bytecode level, and thus serve as an initial test of this technique. (Similarly, `{&compile}` might do well to translate compiled code into a secure-hash source, and then to avoid compiling code that consists only of invocations.)

If I do this well, I shouldn't need to recompile the whole dictionary on each change... only a small subset that is both used and precompiled. This should support effective performance and perhaps better reuse of code and memory. 

