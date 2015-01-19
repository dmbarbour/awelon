
Operator `M`, the merge operator, has a type that is not like the others:

        M :: (a + a') * e → (a * e), assuming future compatibility of a and a'

This isn't quite accurate. In general, we may need more sophisticated types - dependence or existentials. However, I'm not sure how to annotate these in general. Something like: 

        M :: ((a+_)*eL)|((_+a')*eR) → (a*e)

The result of merge is most obvious for cases like `condSelect :: (a+b) x y -- a x OR b y`. These might form existential pairs, such as: `text [text→number] OR number [number→number]`. Long term, use of `M` should recover support for sophisticated options. However, where possible, I'd like to treat it as if it has a simplistic type `(a+a)*e → (a*e)` for some simple structural notions of type `a`.

A simplistic structure for most merges could simplify type inference for much AO code. 
