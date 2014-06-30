
Awelon Bytecode (ABC) has always been intended for compilation (both static and dynamic) to support performance. 

Compilation is also feasible at the Awelon Object (AO) layer. However, compilation at the ABC layer is better for streaming and for achieving my long-term goals. That said, it seems reasonable that developers should be able to guide compilation via the AO layer, either by annotation or by naming conventions.

Use of annotations to guide compilation has and advantage and disadvantage of being potentially dynamic. Names to guide compilation, however, would be static in a very simple way. 

## The Compiler

At the moment, `ao` and `aoi` can utilize the Haskell `plugins` package to load code written in Haskell as a plugin. The idea is to compile ABC to (presumably) efficient Haskell, leverage the Haskell optimizer, and load it as a plugin. Note: the performance at the moment is not very good. I might need to specialize for a specific runtime model, rather than abstract across runtimes. 

Currently, there is an ABC to Haskell compiler written in Haskell, but this is a problematic state of affairs. First, it requires recompiling the Haskell code in order to update the compiler. Second, it doesn't generalize well to cross-compilation to other target languages (e.g. C, OpenCL). Long term, I'd like to wean Awelon project entirely off of Haskell.

At the moment, the implemented compiler is not a very good compiler. There are many optimizations it does not perform, the handling of sum types is rather awkward, and (at the moment) the performance of the compiled code is not considerably better than that of the interpreted code. Indeed, I wouldn't lose much by deleting the current compiler for rewrite purposes.

I aim to rewrite the compiler in AO, and bootstrap properly, but stick with the plugins model for now.

## Controlling Compilation

At the moment, dynamic compilation is supported by use of an `{&compile}` annotation. My idea for static compilation is to target AO words that start with `#`, i.e. such that `#foo` is implicitly compiled. This should offer AO developers some effective, ad-hoc, and relatively transparent support for compilation.

ABC already has an approach to model separate compilation and linking: we use `{#secureHashOfBytecode}` to identify a resource by its secure hash and logically load it in place. This is a simple approach that is compatible with distributed systems and streaming bytecode. 

In context, it may be worthwhile to explicitly use the `{#secureHashOfBytecode}` technique for `{&compile}` and `#foo` compilations. This could help prepare Awelon project for distributed programming. Further, it may mitigate redundant storage and processing of code due to common reuse of words.

## Multiple Compilers?

An interesting possibility is to support multiple compilation words in the dictionary, and perhaps to compile programs multiple times and compare the results. I'm not sure how I'd want to expose this to users, yet, so more development of this idea is necessary.


