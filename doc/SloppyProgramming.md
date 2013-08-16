
NOTE: I'm not sure I'll go for this recursive approach. 

It is very expressive, but it's also difficult to reason about performance of the search. I would like to support Awelon as an effective distribution language WITHOUT pre-compiling it. 

## Compile Time Metaprogramming in Awelon

Awelon supports a simple but expressive form of type-driven metaprogramming based on overloading. A word in Awelon may possess multiple meanings. These different meanings may have different types. At compile time, one meaning is selected for each use of a word, with a goal that the application pass all static checks (both typechecking and static assertions). 

This technique enables Awelon developers to express adaptive software components, robust to extension, reusable in more contexts. It also is the basis for generic programming in Awelon. As a simple case study, consider dup, defined in terms of its primitives and some type-driven search:

        dupStatic :: (Static x) ~> (Static x * Static x) -- prim
        dupAtomic :: (Atomic p dt x) ~> (Atomic p dt x * Atomic p dt x) -- prim

        -- zip2 :: (x1 * x2) * (y1 * y2) ~> (x1 * y1) * (x2 * y2)
        zip2 = assocrp rot3 rot2 assoclp

        dup =| dupStatic  
        dup =| dupAtomic
        dup =| [dup] head [dup] tail zip2

        -- copy corresponds to FORTH's dup
        copy = [dup] first assocrp

With this definition, dup can be applied to both primitives and to complex `*` signals. Of course, this definition is incomplete: it doesn't handle `+` types at all. Fortunately, overloaded definitions support *open extension*: extensions may be applied to an imported definition, and will automatically be integrated to any recursive structure. A developer who later sees this missing feature may transparently introduce it.

*NOTE:* Definitions may only be overloaded if they're defined with `=|`. This constraint exists for visibility reasons: developers should know at a glance whether or not they see a complete definition. Also, only `=|` definitions may be recursive.

### Keyword Arguments in Awelon

Awelon's overloading is expressive enough to represent special variables, keyword arguments, extensible records, and so on. Usefully, it's all free at runtime! What we represent here is a compile-time computation that builds the data-plumbing to operate on a particular element.

The first thing we must do is decide how to represent these associations. There are actually quite a few factors one might consider: syntactic convenience, ease of manipulations (add key, rename key, remove key, drop by key, operate on signal, etc.), effective support for hierarchical representations (e.g. a record of records), and so on. One of the easier to manipulate representations is simply to use a list of `("keyword",argument)` pairs. I.e. instead of `(x * (y * (z * e)))` we might have an assoc list similar to `(("kw1" * x) * (("kw2" * y) * (("kw3" * z) * e)))`. 

One useful manipulation is to load the argument to the top of the stack. 

        -- loadkw :: ("kw" * env) ~> (x * (env-("kw",x)))
        loadkw =| matchkw
        loadkw =| rot2 [loadkw] tail rot2

        -- matchkw :: ("kw" * (("kw" * x) * env) ~> (x * env)
        matchkw = assoclp [assoclp assertEq first] first 

        -- assertEq :: (Static x * Static x) * y ~> y 
        assertEq = [eq] first assertStatic

        assertStatic :: (Static Boolean * x) ~> x -- prim
        eqStaticString :: (Static String * Static String) ~> Static Boolean -- prim
        eq =| eqStaticString -- eq can compare static strings
        eq =| eqStaticInt -- eq can compare lots of things

The only new trick here is the primitive assertStatic, which enables developers to perform an ad-hoc compile-time checks on static values. If the check fails (e.g. when the wrong keyword is compared by matchkw) it is treated the same as a type error, and loadkw will be forced to search further. 

The above implementation is non-deterministic if there happens to more than one element using a keyword. This is important: `=|` does NOT imply any ordering. This isn't necessarily a problem: if the same keyword is used for signals of different types, the decision will be disambiguated downstream. Nonetheless, I imagine many developers would feel *uncomfortable* not knowing precisely which element is selected. 

Fortunately, this can be solved easily. We can modify the second loadkw meaning to assert the first `("keyword",argument)` pair is NOT a match before looking further. Basically, that gives us traditional name shadowing. (Left as an exercise for the reader.)

### PICK and ROLL

In FORTH, common stack operations are copying an element to the top, or moving it to the top. Rather than writing out a different word for every depth, we might choose to abstract the depth argument into a parameter. In FORTH, this results in the operations PICK and ROLL. In most use cases, the depth argument is static. Awelon can model pick and roll with static arguments.

        -- ifEq0 :: (Static Integer * x) ~> x
        -- validate value = 0, remove it
        if0 = [0] first assertEq
  
        -- ifN-- :: (Static Integer * x) ~> (Static Integer * x)
        -- validate value > 0, decrement it
        ifN-- = [copy 0 assertGreater decrement] first

        -- pick will copy the Kth element to the top of the stack. 
        pick =| if0 first assertEq copy
        pick =| ifN-- rot2 [pick] tail rot2

        -- roll will rotate the Kth element to the top of the stack, removing it.
        roll =| if0 -- done
        roll =| ifN-- rot2 [roll] tail rot2


### Sloppy Programming

Keywords, pick, roll, dup, head, tail, etc. are all techniques for *precise* programming - they work well when developers know exactly what they want (which is often true for programming in-the-small). However, there is also a valuable role in this wide world for *sloppy* programming: rapid prototyping and executable pseudocode, plug-and-pray integration efforts, auto-configuration against available features, and live programming environments where active developers can easily gain feedback and refine on-the-fly.

Awelon's overloading supports sloppy programming. The use of `=|` is non-deterministic at compile time. It's easy to get sloppy with non-determinism. As noted with the keywords example, it actually takes more effort to avoid it.

But non-determinism is an unsatisfying foundation for sloppy programming. We aren't seeking just any random solution. We want to find 'good' solutions - those with nicer features, greater performance, or tighter integration. Developers need some way of expressing what 'good' means such that it can help direct searches. 

This isn't a new problem; it has been addressed by *weighted* logics (cf. [dyna](http://www.dyna.org/)) and soft constraint models. The essential idea is to integrate developer-provided scoring with the decision process, and allow this to drive search. Even if an optimal solution is not discovered, a good solution might be - and certainly better than we're likely to discover without guidance.

Awelon supports a flexible scoring mechanism. When an application is compiled or searched, it generates a set of ad-hoc user-defined tags (strings) with attached scores (numbers - count, min, max, sum, sum of squares). The scoring is performed using a static operation, for which I'm currently using the `#` symbol:

        # :: (Static Rational * (Static String * x)) ~> x -- primitive
        option =| "use-gtk" 1 # "experimental" 10 # experimental-gtk-impl

The meaning of this record is then left to the external developer, who is free to express preferences by developing an ad-hoc heuristic scoring function. For example, I like gtk but another developers might favor qt or wx. Awelon thusly enables a valuable separation of policy (who prefers what) from implementation.

Of course, there is also some utility in de-facto standard, common attributes that most people will favor or disfavor. The use of "quality" will serve in that role. For example, if we're attempting to model intelligent 'auto-wiring', we should strongly favor solutions that are *tightly bundled* (i.e. complex types or records stay together) and *nearby* (i.e. on the assumption that developers at least new approximately where to integrate the component). Since these qualities would almost always apply to any developer, we should feel free to use "quality" when describing the quality of a match.

Scores are NOT available within the Awelon application. (That would be problematic because scores are manipulated by statics within the program.)

### Literal Coercions

Awelon allows implicit operations for strings, integers, and rationals that are embedded in source code. This is achieved by implicitly injecting the appropriate coercion operation just after the literal. The default set of coercions is available in the std module, and is user-extensible.

        -- in module std 
        from_str =| id
        from_int =| id
        from_int =| staticIntToRational [from_rat] head -- int to rational to ?
        from_int =| 0 eqStaticInt not -- int to boolean
        from_rat =| id

The use of an integer will implicitly inject the appropriate coercion code. I.e. if you write `1`, Awelon will expand it to `1 [std:from_int] head`. This is a special case; it will work even if the std module is not imported. The motivating case for literal coercions was translating integers to rationals. However, developer extensions could turn this into a powerful feature, especially the use of compile-time string processing.

### No Termination Guarantee

A weakness of Awelon's metaprogramming is the lack of formal guarantee for termination. Type safety is still compositional, but there is no guarantee that a safe solution will be discovered, and in case of type error there is no guarantee the inability to match a type will be recognized.

Fortunately, developers can do much to mitigate any potential issues. It is possible to assert type properties (e.g. type-restricted identity functions) 

Matching types should not be very difficult, however. They have static size and structure. 

Awelon makes many nice guarantees. Type safety is compositional, for example, despite the use of overloading. However, a

This concern is mitigated in part by the fact that its a *compile-time* concern. Developers can be made aware of the search efforts, e.g. by rendering search trees, and can go back to simplify code, or inject some extra type assertions. 

Awelon developers may also *partially compile* certain software components, i.e. freezing parts of the implementation and reducing their type interfaces. When Awelon is used in its role as a distribution language, Awelon code will often be partially frozen in this manner. 


