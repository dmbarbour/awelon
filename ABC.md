
See AboutABC for full explanations and design. This file just records each code, and a pseudocode representation of its type.

        [] :: (special - block reader mode)
        "~ :: (special - text reader mode)
        {} :: (special - capability reader mode)
        (|) :: (special - AMBC; extension to ABC)
        SP,LF :: x → x

        l :: (a * (b * c)) → ((a * b) * c)
        r :: ((a * b) * c) → (a * (b * c))
        w :: (a * (b * c)) → (b * (a * c))
        z :: (a * (b * (c * d))) → (a * (c * (b * d)))
        v :: a → (a * 1)
        c :: (a * 1) → a
        % :: (Droppable x) ⇒ (x * e) → e
        ^ :: (Copyable x) ⇒ (x * e) → (x * (x * e))

        $ :: [x→x'] * (x * e) → (x' * e)
        o :: [y→z] * ([x→y] * e) → [x→z] * e
        ' :: (Quotable x) ⇒ (x * e) → ([s→(x*s)] * e)
        k :: ([x→y] * e) → ([x→y]k * e) -- keep, relevant, no drop
        f :: ([x→y] * e) → ([x→y]f * e) -- affine, no copy

        # :: e → (N(0) * e)
        0 :: N(x) * e → N(10x+0) * e
        1 :: N(x) * e → N(10x+1) * e
        2 :: N(x) * e → N(10x+2) * e
        3 :: N(x) * e → N(10x+3) * e
        4 :: N(x) * e → N(10x+4) * e
        5 :: N(x) * e → N(10x+5) * e
        6 :: N(x) * e → N(10x+6) * e
        7 :: N(x) * e → N(10x+7) * e
        8 :: N(x) * e → N(10x+8) * e
        9 :: N(x) * e → N(10x+9) * e

        + :: N(a) * (N(b) * e) → N(a+b) * e
        * :: N(a) * (N(b) * e) → N(a*b) * e
        / :: N(non-zero a) * e → N(1/a) * e
        - :: N(a) * e → N(0-a) * e
        Q :: N(non-zero b) * (N(a) * e) → N(r) * (N(q) * e)
            where qb+r = a, q integral, r between 0 and b (excluding b)

        L :: (a + (b + c)) * e → ((a + b) + c) * e
        R :: ((a + b) + c) * e → (a + (b + c)) * e
        W :: (a + (b + c)) * e → (b + (a + c)) * e
        Z :: (a + (b + (c + d))) * e → (a + (c + (b + d))) * e
        V :: a * e → (a + 0) * e
        C :: (a + 0) * e → a * e

        ? :: (Droppable b) :: b@[a→a'] * ((a+b)*e) → (a'+b)*e
        D :: a * ((b+c) * e) → ((a*b) + (a*c)) * e -- distrib
        F :: ((a*b) + (c*d)) * e → (a+c) * ((b+d) * e) -- partial factor
        M :: (a + a') * e → a * e -- merge; a and a' compatible
        K :: (a + b ) * e → b * e -- assert; must be in b

        P :: (Observable x) ⇒ x * e → (x+(a*b)) * e -- x is pair?
        S :: (Observable x) ⇒ x * e → (x+(a+b)) * e -- x is sum?
        B :: (Observable x) ⇒ x * e → (x+[a→b]) * e -- x is block?
        N :: (Observable x) ⇒ x * e → (x+N(a)) * e -- x is number?
        > :: (Comparable x y) ⇒ x * (y * e) → ((y*x)+(x*y)) * e -- y > x
            #4 #2 > -- asks whether 4 > 2. Returns (N(2)*N(4)) on right.

Legend for types: `*` is a product or pair, `+` is a sum or Either type, `[x→y]` is a block type that can map from type `x` to type `y`, `N(x)` indicates a number with value x (numbers should be tracked in types as much as possible).

Text is modeled is a list of small natural numbers (in range 0 to 1114111) terminated by number 3. E.g. the word "Text" corresponds to `#3#116l#120l#101l#84l` (constructed back to front). 

Aside: The design of ABC is avoiding vowels, to avoid spelling naughty words. It isn't a strong design constraint, and I have used `o` due to visual similarity with the traditional function composition operator. Also, ABCD begins at U+00C0.

## ABCD

None yet!

## TEMPORARY ABC

If I get stuck too long on a tricky function that hinders real progress on higher level development (bootstrapping, etc.), I can implement a temporary version as an ABC primitive. This is not a desirable condition, and should only be used for essential functions.

Currently, a loop primitive is provided as temporary ABC. 

        ∞ :: [a→(a+b)]*(a*e)→(b*e)              until1 primitive
          code is U+221E
          requires copyable, droppable block

I'm tempted to also provide primitives for `each` or `map` on lists, in part because it's common and easy to reason about termination, and also likely to become an ABCD element anyway. Potential:

        Σ :: [(a*e)→e']*(a`L*e)→e'              each primitive
          code is U+03A3
          requires copyable, droppable block

Loops seem tricky to implement by fixpoint, in the absence of any primitives beyond copy and compose. So far, I've only been able to implement one loop in ABC - i.e. `[dup inline] dup inline` in AO, and small variations. I've not figured how to add a useful payload or express terminating conditions. Anyhow, loops to process text and such are essential. If necessary, I would introduce a new primitive to implement them.

Despite being temporary and outside the normal codepoint range for ABC, these are still first-class primitives with respect to compilation. 
