Just a thought: Homotopy Type Theory is looking very promising, an intersection between proof and application. 

Currently, ABC is based on category theory and arrows. But I wonder if HoTT might provide a better basis. Todo: learn HoTT.

(Not a good reason to delay ABC, since an HoTT based version should be easy to translate to if not from.)

Vladimir Voevodsky 

* Types as Sets don't make sense
* Homotypy Theoretic Semantics for Martin-Lof type theories.

Types shouldn't be sets.
Types in Martin-Lof type theory are better thought of as Homotopy Types
Formal semantics of Martin-Lof in Homotopy Types

sets
above sets, categories, then 2-categories
then groupoids, then 2-groupoids... n-groupoids, infinity-groupoids
infinity groupoids ~= homotopy types
mathematics: structures on homotopy types
(yikes, need to climb a big ladder of abstraction and learn how much further I can see)

I would need "invertability" - every operation uniquely reversible, given one of the two inputs - to have groupoids. Perhaps this is doable. It seems that integers have this property, and so do text ops.

Products might need to be replaced by fibrations - i.e. something weaker than products that enables moving sideways.

Sheafs look interesting... 

I would very much like to have all 'values' be types, and meaningful behaviors. If this happens early enough, I could modify ABC. Otherwise, I might need to upgrade the language, which would be annoying.

(Basically, ABC should be a streaming proof that transforms one type to another.)

The only exception is drop, first, and capabilities. Or perhaps I can make it so these are the only exceptions. OTOH, it might be that 'drop' is also okay; the inverse function would be specific to the dropped object, though.




This would probably lose integer ops, some text ops, certainly the ability to drop anything. But it may be acceptable to introduce these operations in a controlled way.




