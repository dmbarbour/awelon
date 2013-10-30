
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

        " :: (special - text reader mode)
        [ :: (special - block reader mode)
    
Here `*` as a type means product (pair), `[x->y]` is a block type, `T` means text, `N` means number.





