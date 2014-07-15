
Awelon Bytecode (ABC) has always been intended for compilation (both static and dynamic) to support performance. 

Compilation is also feasible at the Awelon Object (AO) layer. However, compilation at the ABC layer is better for streaming and for achieving my long-term goals. That said, it seems reasonable that developers should be able to guide compilation via the AO layer, either by annotation or by naming conventions.

Use of annotations to guide compilation has and advantage and disadvantage of being potentially dynamic. Names to guide compilation, however, would be static in a very simple way. 

## The Compiler

At the moment, `ao` and `aoi` can utilize the Haskell `plugins` package to load code written in Haskell as a plugin. The idea is to compile ABC to (presumably) efficient Haskell, leverage the Haskell optimizer, and load it as a plugin. Note: the performance at the moment is not very good. I might need to specialize for a specific runtime model, rather than abstract across runtimes. 

Currently, there is an ABC to Haskell compiler written in Haskell, but this is a problematic state of affairs. First, it requires recompiling the Haskell code in order to update the compiler. Second, it doesn't generalize well to cross-compilation to other target languages (e.g. C, OpenCL). Long term, I'd like to wean Awelon project entirely off of Haskell.

At the moment, the implemented compiler is not a very good compiler. There are many optimizations it does not perform, the handling of sum types is rather awkward, and (at the moment) the performance of the compiled code is not considerably better than that of the interpreted code. Indeed, I wouldn't lose much by deleting the current compiler for rewrite purposes.

I aim to rewrite the compiler in AO, and bootstrap properly, but stick with the plugins model for now.

## Controlling Compilation

At the moment, dynamic compilation is supported by use of an `{&compile}` annotation, of identity type on blocks `[a→b]→[a→b]`. I'm well enough satisfied by this.

I am also interested in supporting 'static' compilation, per word in the dictionary. Per-word compilation can potentially mitigate exponential expansion overheads associated with AO's 'inline everything' model, and jumpstart early use of external bytecode resources.

The question I have is how to best guide per-word compilation.

Use of a prefix (e.g. words starting with `#` are compiled) initially appealed to me, but with hindsight it's hard to understand why. A tight coupling between names and performance is very painful during development, when we're trying to tweak the performance. Also, it would not compose well if we tried to use the same convention to guide other aspects of implementation.

So, the guidance should be separate from the word itself. The most promising possibility is to simply define `compile!foo` for every word `foo` we wish to compile (ignoring the actual contents of `compile!foo`). 


## Multiple Compilers?

An interesting possibility is to support multiple compilation words in the dictionary, and perhaps to compile programs multiple times and compare the results. I'm not sure how I'd want to expose this to users, yet, so more development of this idea is necessary.
