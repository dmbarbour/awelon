Regarding Long-term Compression of ABC Data

Another document, [Compression.md](Compression.md), describes potential for deterministic, streaming compression of ABC resources and control streams. But that's a very small window, about 2k elements. 

At a much larger scale, we can potentially compress by use of ABC resources. An ABC resource identifier compresses to 53 bytes in a stream (accounting for the binary compression pass). If said identifier refers to 2kB of bytecode, the compression factor is about 38x per use. Potentially, we could be referring to resources much larger than 2kB. The large identifier isn't really a problem, so long as the named target is much larger.

At the medium scale, ABCD is an interesting possibility. ABCD makes use of higher, valid UTF-8 characters, giving them meaning according to a global dictionary. That gives us a maximum dictionary of about 1M elements. But, realistically, we'll reach perhaps 4k-16k functions. If these functions are well chosen, then ABCD could offer reasonable compression benefits. The primary use of ABCD is to improve interpreter performance, by allowing specializations of common functions.

Can we do better for compression at the medium scale? E.g. assuming *gigabytes* of ABC data on a virtual machine, it might be useful to compress locally. 

Awelon Object code is one interesting approach to compress ABC. But it is not ideal, since AO is for human use and thus doesn't readily account for partial evaluation, nor for stability.

But something like an AO dictionary could be developed by machine. And it would not even be very difficult... we could easily use something like `@word ` in an ABC stream to refer to a word defined in the local dictionary, treating `@` as a stream escape. We could actually do this at the UTF-8 layer with something like `0xFD + variable width integer`. 

Building a large, local dictionary is quite feasible. We could potentially form an automatic dictionary of many millions of elements, based on analysis of repeating code or data within large systems, and essentially share an intermediate language involving ad-hoc escapes into an implicit (but not global) dictionary.
