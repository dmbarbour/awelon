
See AboutABC for full explanations and design. This file just records each code, and a pseudocode representation of its type.

## Operators and Literals

        [] :: (special - block literal)
        "~ :: (special - text literal)
        {} :: (special - token or capability; see below)
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
        o :: [x→y] * ([y→z] * e) → [x→z] * e
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

        > :: N(x) * (N(y) * e) → ((N(y)*N(x))+(N(x)*N(y)) * e -- y > x
            #4 #2 > -- observes 4 > 2. Returns (N(2)*N(4)) on right.

Legend for types: `*` is a product or pair, `+` is a sum or Either type, `[x→y]` is a block type that can map from type `x` to type `y`, `N(x)` indicates a number with value x (numbers should be tracked in types as much as possible). 

Text is modeled is a list of small natural numbers (in range 0 to 1114111). Lists are modeled using a structure of form `µL.((elem*L)+1)`. 

Aside: The design of ABC is avoiding vowels, to avoid spelling naughty words. It isn't a strong design constraint, and I have used `o` due to visual similarity with the traditional function composition operator. Also, use of `@` (64) and backquote (96) will be avoided to support hosting ABC in external streams or texts.

## Tokens and Capabilities

Tokens in ABC are simply expressed using text between curly braces, `{foo}`. The token is unforgeable from within ABC, and the text is often cryptographically secure from outside ABC. A capability can be expressed by wrapping a token in a block, e.g. `[{foo}]`. When invoked, the token's text is passed to the current environment to determine its effect. In many cases, capability text is specific to an environment. 

There are some common conventions based on prefix characters. For example:

        {&foo}  :: a → a                - annotation capability

        {:foo}  :: (a*e) → ((foo:a)*e)  - discretionary sealer
        {.foo}  :: ((foo:a)*e) → (a*e)  - discretionary unsealer

        {:format$leftKey}               - cryptographic sealer
        {.format$rightKey}              - cryptographic unsealer
        {$format}                       - indicates sealed value

        {#kjhfmskpzgzdqbqstdh...}       - ABC resource

Annotations can support debugging and performance. Discretionary value sealing act as structural type tags. Cryptographic sealers support rights amplification and other security patterns. ABC resources are named by a combination of secure hashes to look up and decrypt the bytecode; they serve a valuable role in separate compilation and dynamic linking, and for saving bandwidth and storage at larger scales.

## Existing Annotations

While annotations are not strongly standardized, it's nice to avoid conflicts and reuse annotations where feasible. I expect some de-facto standards to emerge. Here are some annotations currently in use:

        {&≡}  (that's U+2261) - (a*(a*e))→(a*(a*e)); assert structural equality
        {&static}             - (a*e)→(a*e); try to compute `a` at compile time
        {&asynch}             - ([a→b]*e)→([a→b]*e); compute block in parallel
        {&compile}            - ([a→b]*e)→([a→b]*e); optimize performance for block

        {&debug print raw}    - print ad-hoc input to error console (for human) 
        {&debug print text}   - print textual input to error console (for human)

In general, annotations may be ignored by an environment that doesn't recognize them, and are discretionary within environments that do recognize them. Annotations always have an identity type, and should have no observable impact on a correct program's behavior (modulo performance, debugger integration, etc.). 

## Binaries in ABC

Binaries are encoded in ABC using the base16 alphabet, `a-z` minus vowels `aeiou` and common data plumbing operators `vrwlc`. This encoding doubles the natural size of the binary, but then we will apply a specialized compression pass just for large sequences of base16, reducing them back to binary with just a little overhead (0.8% for large binaries). Compression is performed for ABC resources, cryptographically sealed values, and most ABC streams over a network.

Binary compression is followed by a more conventional compression pass, but the worst case total overhead is guaranteed to be less than 2.5% for uncompressible large binaries. 

## ABC CHANGE LOG

March 2014: 
* eliminated operators `PSBN`, which would observe type information

April 2014: 
* made `>` monomorphic, so it operates only on a pair of numbers
* modify list type from `µL.(1+(elem*L))` to `µL.((elem*L)+1)`

August 2014
* swap order of arguments to operator `o`, to better match common use

## ABCD

None yet! ABCD will begin after U+00C0, and will develop according to empirical analysis of common subprogram patterns that offer effective compression and optimization benefits. Naturally, ABCD won't make any progress at all until we have a much larger and more mature ABC systems.
