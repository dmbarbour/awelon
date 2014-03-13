
See AboutABC for full explanations and design. This file just records each code, and a pseudocode representation of its type.

        [] :: (special - block reader mode)
        "~ :: (special - text reader mode)
        {} :: (special - capability reader mode)
        (|) :: (special - AMBC; extension to ABC)
        SP,LF :: x → x (whitespace as identity)

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

        ? :: (Droppable b) :: b@[x→x'] * ((x+y)*e) → (x'+y)*e
        D :: a * ((b+c) * e) → ((a*b) + (a*c)) * e -- distrib
        F :: ((a*b) + (c*d)) * e → (a+c) * ((b+d) * e) -- partial factor
        M :: (a + a') * e → a * e -- merge; a and a' compatible
        K :: (a + b ) * e → b * e -- assert; must be in b

        > :: (Comparable x) ⇒ x₁ * (x₂ * e) → ((x₂*x₁)+(x₁*x₂)) * e -- x₂ > x₁
            #4 #2 > -- observes 4 > 2. Returns (N(2)*N(4)) on right.

        {:foo} :: a → Sealed foo a      -- sealer capability
        {.foo} :: Sealed foo a → a      -- unsealer capability
        {&foo} :: a → a                 -- annotation capability

Legend for types: `*` is a product or pair, `+` is a sum or Either type, `[x→y]` is a block type that can map from type `x` to type `y`, `N(x)` indicates a number with value x (numbers should be tracked in types as much as possible).

Text is modeled is a list of small natural numbers (in range 0 to 1114111). Lists are modeled using a structure of form `µL.(1+(a*L))`. 

Comparisons with `>` are, by design, just flexible enough to directly compare texts (ensuring that texts are suitable as identifiers). Four basic types can be directly compared: numbers with numbers (numerical order), sums with sums (left is less than right), products with products (first before second), and unit with unit (always equal). The goal here is to ensure texts are first-class, suitable for use as identifiers. Blocks and sealed values may not be compared.

Static sealers and unsealers are, by nature, relatively insecure. However, for use in open systems, they'll often be rewritten using HMAC or similar to secure them. 

Aside: The design of ABC is avoiding vowels, to avoid spelling naughty words. It isn't a strong design constraint, and I have used `o` due to visual similarity with the traditional function composition operator. Also, ABCD begins at U+00C0.

## ABCD

None yet!

## Desiderata

I would like to find simpler alternatives for `F` and `M` operators, with respect to static analysis. Suggestions are welcome.

## Deprecated

Originally, ABC had some introspection operators.

        P :: (Observable x) ⇒ x * e → (x+(a*b)) * e -- x is pair?
        S :: (Observable x) ⇒ x * e → (x+(a+b)) * e -- x is sum?
        B :: (Observable x) ⇒ x * e → (x+[a→b]) * e -- x is block?
        N :: (Observable x) ⇒ x * e → (x+N(a)) * e -- x is number?

However, these are being removed because they introduce a lot of complexity for static analysis, without offering significant benefits. In addition, the older definition of `>` would apply to many types, not just numbers.

