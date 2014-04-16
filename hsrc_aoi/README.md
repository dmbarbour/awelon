
The `aoi` executable provides a simple REPL environment for interactive development and testing of AO code. The environment provides a prompt, and a simple history. Developers may reload the dictionary without exiting the environment by tapping Ctrl+C at an empty prompt.

While seeking a unification with `ao`, it seems my support for 'undo' is at least temporarily disabled. Rather, if I want 'undo', I'll need to support it in `AORT`, i.e. the multi-agent and effects layer. OTOH, it could be quite useful to do so. I might also need to adjust how the client interacts with `AORT`, or add a new layer of indirection such that the user itself is modeled as an undo-able resource (a user agent).

For now, I'll just accept the loss of 'undo'. Might be better to target that with RDP, anyway.
