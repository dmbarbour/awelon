
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

The question I have is how to best control per-word compilation. Some possibilities:

* Use a prefix, e.g. `#` so `#foo` is implicitly targeted for compilation
* Define a separate preCompiledWords file or definition
* Annotate the `foo` definition for compilation
* Define `compile.foo` for each `foo` that I wish to precompile
* Heuristically infer words for compilation

Use of a prefix initially appealed to me, but with hindsight it's hard to understand why. It seems a tight coupling between names and performance is very painful during development. Use of a separate file seems like it might be disadvantageous long-term, and is contrary to my dictionary-as-OS concept. Use of a single compilation word centralizes a lot of management, and thus requires careful administration. Annotations don't really have a well-defined scope as components, which can make them more difficult to process.

It seems to me the best option is to focus on per-word compilation, and perhaps the heuristic approach.

An interesting possibility is to automatically combine these. I could try to use a word like `compile.foo` to *modify* the compilation heuristic in the particular case of `foo`. 

## Multiple Compilers?

An interesting possibility is to support multiple compilation words in the dictionary, and perhaps to compile programs multiple times and compare the results. I'm not sure how I'd want to expose this to users, yet, so more development of this idea is necessary.
