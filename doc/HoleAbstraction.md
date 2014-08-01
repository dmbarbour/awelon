In "A Functional Representation of Data Structures with a Hole" by Yasuhiko Minamide (1998), the idea of 'data structures with a hole' is presented as a variation on the lambda abstraction. 

The implementation is a pair: a data structure and its hole. Affine typing is used to ensure the hole is filled at most once. And the lambda abstraction prevents observation of the data structure until the hole is filled, thus avoiding some problems with logic variables or single-assignment variables causing remote communication. 

This is an interesting possibility. I'm not at all convinced that the added sophistication is worth lifting into the ABC core semantics (which aim for simplicity and minimalism). But I wonder whether the idea could be adapted outside of semantics, e.g. via annotation of an affine block. When we know a block is affine, we presumably can pre-allocate most of the object it would construct, and perhaps achieve something similar.

The idea, unfortunately, isn't an ideal fit for ABC; lambda abstractions already have a semantics that 'teleport' information into the right location, replacing a named variable, so it makes sense that we could have a O(1) special implementation of this. ABC, on the other hand, models precisely how we walk a structure to inject information... so adding information 'deep' in a structure doesn't happen in O(1) time; in ABC we must instead surgically open it.

At this point, it is likely more profitable to focus on finger-trees and huet zippers as ways of efficiently manipulating structure.

*Aside:* Lazy or asynchronous evaluation may serve the same role pretty well in most cases, and are relatively easy to achieve through annotation.
