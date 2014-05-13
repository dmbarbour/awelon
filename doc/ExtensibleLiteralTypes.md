
AO supports literal numbers and text

        42
        3e9
          "Hello, world!"
        "multi line 
         text
        ~

But I can imagine I'd like to support many ad-hoc literals in the future, especially as I move towards live programming: canvases, sliders, matrices, colors and color pickers, and so on.

It would not be difficult to use ad-hoc literal types through the IDE, then generate the appropriate AO code on-the-fly. This would essentially be a language layer above AO. However, I think it would be best to standardize this such that AO code written using such literals can be viewed and edited in a consistent manner across IDEs and browsers.

Hmm.

Perhaps it won't matter, ultimately, if I instead take the direction of deprecating AO in favor of the tools/hands and lenses/eyes applications and unified UI/PL model. AO has always been intended as a temporary language. But even so, it might be nice to operate on rich 'objects' in a manner compatible with embedding in an AO stream in context of a wiki-based IDE or similar. 

Thoughts:

There should either be some annotations or a naming convention involved to somehow 'announce' to the IDE that a user-defined literal view should be invoked. Annotations go all the way down to the ABC, and it might be nice if I can view literals even in the ABC (similar to how text and numbers are visible). However, it might be preferable that I don't embed expensive view-code in the generated ABC.

A literal must be isolated within the 'source code' of AO. That is, a literal is not simply a partially evaluated object or value; it should correspond to a finite sequence of the stream itself, and generally behave independently of usage context (though, that might be convention more than requirement). 

For more sophisticated literals, I might want to argument the 'view' of the literal independently of manipulating the literal itself. Hmm. So literals might need a few different independent attributes. But this probably isn't critical; I could manage the view and literal together, so long as I have an appropriate extractor function that can access the literal independently of the view.

## Potential Approach

My first thought for approach is to use a simple `literal.` naming convention. A user-defined literal in the AO code consists of a pair of form:
    
        [a block of code] literal.foo

The compatible IDE will hide or fade the block and the word, perhaps keeping them accessible in the background or in a separate widget. Instead, rendering of this literal pair will be guided by supplementary words such as `render.literal.foo` or perhaps `ui.literal.foo`. These words would be applied in a fresh context. (There is a lot of detail work that needs development here.) 

The block associated with the literal will contain all *information* for that literal. Such a block could be constructed by many means:

* it could represent a sequence of attribute updates
* it could represent a stream of 'gestures' manipulating a literal
* it could model a process/object that may receive update and render commands

We don't necessarily want to keep a complete history of actions in the literal itself (text and numbers don't have that feature, for example). So I think the result of an update to the literal would be to rewrite the block, or perhaps to write a fresh one. Of course, it wouldn't be a problem to write a block that maintains history to some degree, e.g. in the 'stream of gestures' metaphor we might mostly extend the existing block.

It seems that literal blocks could reasonably grow very large, without much refactoring, unlikes typical AO code. This is probably a good thing: it fulfills a useful role to support large constructs without extensive refactoring. Nothing else in AO can do so effectively.



We might generalize a little. Instead of just blocks, we could also support `42 literal.foo` and `"text" literal.foo`. Fundamentally, however, a block is more general than either of these options, and vastly more extensible. So I think it's better to stick with `[block] literal.foo` sequences for now.



