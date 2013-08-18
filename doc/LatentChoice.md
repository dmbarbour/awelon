
This is an idea that is incomplete.

## Static Latent Choice

Awelon also provides a static type `(x & y)` that offers a latent choice (aka 'additive conjunction' in linear logic, or just 'offer' in Awelon). The intuition is that we are offering x or y, but the choice hasn't been made yet. The utility of latent choice comes from a programmer's ability to continue extrapolating on the different paths before making a choice. 

Latent choice is useful for modeling lookahead searches, or for adaptive software where we want to examine multiple valid static outcomes and pick a 'best' one according to some static heuristic. Simpler searches, e.g. scanning an association list of `(Static Text * value)` pairs, don't require latent choice. Since latent choice can be a relatively expensive compile-time feature, it should be avoided if unnecessary. 

        offerFork   :: x ~> (x & x)
        offerAccept :: (x & y) ~> x
        offerDist   :: (x * (y & z)) ~> ((x * y) & (x * z))
        offerLeft   :: Static (x ~> x') * (x & y) ~> (x' & y)
        offerApply  :: (Static (x&y)~>z) * (x&y) ~> z

Note that developers cannot create i
 
Because RDP behaviors are effectful, latent choice doesn't have a corresponding runtime type.

 latent choice is only feasible with static computations - because it's impossible to undo effects on 

 we must choose only one. The value of a latent choice is that we can continue to extrapolate both paths, and only make a choice after we know which path is "best". Latent 

 The intuition is: an upstream developer can *offer* multiple options, and the downstream developer must select exactly one (by eliminating a bad offer, or picking a good one). Offerings can express the tail-end of a searches with path-elimination - which is important when modeling features like searching for a named stack in the environment (i.e. when we don't know what program we need until after we've built it). 

Offerings can express static searches in a relatively formal way. 

Offering corresponds roughly to additive conjunction in 

The intuition is: an upstream developer can provide a multiple-choice question, and the downstream developer can (equivalently) pick or eliminate a path. Searches are 



