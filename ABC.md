
See AboutABC for full explanations and design. This file just records each code, and a pseudocode representation of its type.

        [] :: (special - block reader mode)
        "~ :: (special - text reader mode)

        l :: (a * (b * c)) -> ((a * b) * c)
        r :: ((a * b) * c) -> (a * (b * c))
        w :: (a * (b * c)) -> (b * (a * c))
        z :: ((a * b) * (c * d)) -> ((a * c) * (b * d))
        v :: a -> (a * 1)
        c :: (a * 1) -> a
        % :: (Droppable x) => (x * e) -> e
        ^ :: (Copyable x) => (x * e) -> (x * (x * e))

        $ :: [x->x'] * (x * e) -> (x' * e)
        m :: [y->z] * [x->y] -> [x->z]
        ' :: (Quotable x) => (x * e) -> ([1->x] * e)
        k :: ([x->y] * e) -> ([x->y]' * e) (keep, relevant, no drop)
        f :: ([x->y] * e) -> ([x->y]' * e) (affine, no copy)        

        # :: e -> (N(0) * e)
        0 :: N(x) * e -> N(10x+0) * e
        1 :: N(x) * e -> N(10x+1) * e
        2 :: N(x) * e -> N(10x+2) * e
        3 :: N(x) * e -> N(10x+3) * e
        4 :: N(x) * e -> N(10x+4) * e
        5 :: N(x) * e -> N(10x+5) * e
        6 :: N(x) * e -> N(10x+6) * e
        7 :: N(x) * e -> N(10x+7) * e
        8 :: N(x) * e -> N(10x+8) * e
        9 :: N(x) * e -> N(10x+9) * e

        + :: N(a) * (N(b) * e) -> N(a+b) * e
        * :: N(a) * (N(b) * e) -> N(a*b) * e
        / :: N(non-zero a) * e -> N(1/a) * e
        - :: N(a) * e -> N(0-a) * e
        Q :: N(non-zero b) * (N(a) * e) -> N(r) * (N(q) * e)
            where qb+r = a, q integral, r in [0,b) or (b,0]

        ? :: (Droppable b) :: b@[a->a'] * ((a+b)*e) -> (a'+b)*e
        L :: (a + (b + c)) * e -> ((a + b) + c) * e
        R :: ((a + b) + c) * e -> (a + (b + c)) * e
        W :: (a + (b + c)) * e -> (b + (a + c)) * e
        Z :: ((a + b) + (c + d)) * e -> ((a + c) + (b + d)) * e
        V :: a * e -> (a + 0) * e
        C :: (a + 0) * e -> a * e

        D :: a * ((b+c) * e) -> ((a*b) + (a*c)) * e -- distrib
        F :: ((a*b) + (c*d)) * e -> (a+c) * ((b+d) * e) -- partial factor
        M :: (a+a') * e -> a * e -- merge
        K :: (a + b) * e -> b * e -- assert

        P :: (Observable x) => x * e -> (x+(a*b)) * e -- x is pair?
        N :: (Observable x) => x * e -> (x+N(a)) * e -- x is number?
        B :: (Observable x) => x * e -> (x+[a->b]) * e -- x is block?
        S :: (Observable x) => x * e -> (x+(a+b)) * e -- x is sum?
        < :: (Comparable x y) => x * (y * e) -> ((y*x)+(x*y)) * e -- x < y ?

Legend for types: `*` is a product or pair, `+` is a sum or Either type, `[x->y]` is a block type that can map from type `x` to type `y`, `N(x)` indicates a number with value x (numbers should be tracked in types as much as possible).

Text is modeled is a list of small natural numbers (in range 0 to 1114111) terminated by number 3. E.g. the word "Text" corresponds to `#3#116l#120l#101l#84l` (constructed back to front). 

The design of ABC is avoiding most vowels, to avoid spelling naughty or foul words. It isn't a strong design constraint, but it seems worthwhile. 





