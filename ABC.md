
See AboutABC for full description and design. This file just records each code, and a pseudocode representation of its type.

        l :: (a * (b * c)) -> ((a * b) * c)
        r :: ((a * b) * c) -> (a * (b * c))
        w :: (a * (b * c)) -> (b * (a * c))
        z :: ((a * b) * (c * d)) -> ((a * c) * (b * d))
        v :: a -> (a * 1)
        c :: (a * 1) -> a
        % :: (Droppable x) => (x * e) -> e
        ^ :: (Copyable x) => (x * e) -> (x * (x * e))

        $ :: [x->x'] * (x * e) -> (x' * e)
        o :: [y->z] * [x->y] -> [x->z]
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

        

        "~ :: (special - text reader mode)
        [] :: (special - block reader mode)

        (tentative, not committed) 
        Q :: N(non-zero b) * (N(a) * e) -> N(r) * (N(q) * e)
            where qb+r = a, q integral, r in [0,b) or (b,0]
    
Here `*` as a type means product (pair), `[x->y]` is a block type, `T` means text, `N` means number. 

Notes: I'd like to avoid use of `IiO` to keep the code more readable. I'd like to avoid use of most vowels, or relegate them to relatively rare operators, to reduce risk that naughty words are spelled into the ABC. I'd like to avoid reusing `~` for anything since it already terminates a text. 

At the moment, I'm avoiding use of vowels (mostly to avoid spelling naughty things). If necessary, I'll use them for fringe features that aren't used much together.





