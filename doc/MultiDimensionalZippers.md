
Huet's zipper concept is very powerful, and works well for lists and trees. I've been using a structure similar to the following structure for tree zippers:

        zwrap :: x <-> x * (1 * 1) :: zunwrap
        zf :: (x * y) * (l * r) <-> x * (1 * (y * (l * r))) :: zuf
        zs :: (x * y) * (l * r) <-> y * ((x * l) * r) :: zus

Given some of the changes, i.e. that I'm not allowed to introspect types for unit, I'll probably need to use numbers instead of unit type. Perhaps I'll use number 2 for zippers, or perhaps different numbers for different conditions (2, 5 for zwrap, 7 for zf).

Anyhow, zippers work well for trees and lists, but not so well for grids and volumes. But an article I read modeled grids and volumes by modeling intersection of lists:

        213|1|798
        234|3|452
        ---------
        128|9|858
        ---------
        546|6|082
        971|2|877

In this case, we have a list of lists. Moving this list in one dimension, e.g. up and down, is quite trivial (O(1)). Moving it left and right is much less trivial: it requires moving all the lists above and below your position by a step. This could be paid incrementally, but even so it is too high a price.

But it seems feasible that we could make this efficient. 

It would not be difficult to track relative east-west position as an integer and apply only the difference when necessary. This would reduce `east west east west east west` to an NOP with respect to stepping north and south. But by itself it wouldn't be sufficient: if we step `east` 1000 times, then step north once, we'll pay for all 1000 east steps again... albeit, just once, then stepping south and north again would be O(1). The initial cost of moving north-south would be proportional to our distance from origin.

What would be ideal is to have a O(1) cost to step `north` after 1000 steps east.

What would be acceptable is a sublinear cost - preferably lg(N) for the large movement.

The question, then, is how to model an efficiently model lists with the following properties:

* Splitting N elements is O(lg(N)).
* Adding N elements is O(lg(N)).
* Ability to model lazy or procedurally generated structure.
* O(1) cost to manipulate the first element.

It seems to me that finger-trees already have most of the answers here. 

But we can go beyond list-of-lists: consider use of spatial trees to start with, e.g. R-trees or kd-trees or octrees.

I'm going to try creating a blog article on this subject. 

ANYHOW, the application of spatial zippers in Awelon project will be very important for the user interface models and rendering. If I ever figure out multi-agent zippers (perhaps by taking charge of the tree? or perhaps having higher order zippers of some sort?) then zippers might also become a powerful model for collaborative state.



