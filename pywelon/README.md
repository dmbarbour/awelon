
PyPy and RPython has an interesting ability: we can implement an interpreter and obtain a tracing x86 JIT almost for free. This is well explained by Lawerence Tratt in his 2012 article, [Fast Enough VMs in Fast Enough Time](http://tratt.net/laurie/blog/entries/fast_enough_vms_in_fast_enough_time).

This seems interesting. I'll give it a try, so long as it takes at most a few days. :)

This implementation is for a simple ABC interpreter, accepting the ABC stream via standard input. For now, the program starts in AO's standard environment. Eventually, I may add a persistence option, or support paragraphs. For now, the powerblock is quite limited.

So usage may currently look like:

     ao abc command | pypy runABC.py ...

... or similar. I'm not fluent with python, and I'll be learning as I go along. 

If this works out really well, I might need to re-implement my REPL using pypy. It still won't be a good substitute for a proper compiler (there are quite a few dataflow optimizations that I doubt even pypy can perform). But even a 10x performance improvement would be extremely useful.


