
## Adverbs and Inflection (EXPERIMENTAL!)

In AO, we might decide to apply some word `foo` to each element of a list. We can easily express this as `[foo] each`, which would directly apply foo to each element of a list. If we further want to keep a copy of the list, we might modify this to `[[foo] each] keep`. If we also want to hide the first element on the stack, we might modify this to `[[[foo] each] keep] dip`.

An 'adverb' is a word that modifies a verb. 

Words such as `each`, `keep`, and `dip` aren't adverbs. They're too active. But they are at least *related* to adverbs. If we were instead to say `[foo] listwise`, we might expect as result a function - a verb - that, *when later applied to a list*, will apply `foo` to each element in the list. We could define the adverb `listwise` as simply `[each] curry`. Currying relates higher order functions to adverbs. Adverbs have a nice property: their input and output type is the same - just a verb. This makes adverbs very compositional and a good fit for concatenative PLs. We can meaningfully say `[foo] listwise barwise bazwise`, and we can directly refactor or abstract a common sequences of adverbs. 

Unfortunately, `[foo] each` is easier to write than `[foo] listwise apply` even if we ignore the additional one-time cost to define `listwise`. The path of least resistance thus guides developers towards rigid, brittle structures that are difficult to directly abstract or refactor such as `[[[foo] each] keep] dip`. 

To make adverbs usable, I propose syntactic sugar for [inflection](http://en.wikipedia.org/wiki/Inflection). Inflection refers to modifying the structure of a word in a simple, systematic way in order to modify the word's meanings. With inflection, a subprogram of form `[foo] listwise keeping withdip apply` might be expressed parsimoniously as `foo\*kd`. The details:

* character `\` is now reserved, may not be used in normal words
* `foo\*kd` rewrites in place to `[foo] [\*kd] applyWithAdverbs`
* like inline ABC, `\*kd` encodes the sequence of words `\* \k \d`
* users define words `\*`, `\k`, `\d` (etc.) and `applyWithAdverbs`

Developers are limited to one distinguishing character per modifier. However, not every modifier needs to directly represent an adverb. It is feasible to model selectors such that digraph `\K₃` constructs an adverb, or such that `\lf` represents a fold over a list while `\Tf` represents a fold over trees. That said, unicode is big while the number of inflections worth defining for reuse is relatively small. Character per adverb should often serve admirably in practice.

The standard definition for `applyWithAdverbs` will first apply the adverbs to the word in a controlled environment, i.e. such that the adverbs cannot be effectful or sensitive to context, then inline the result. By doing so, `foo\*kd` can easily be understood as a single word.

As an experimental feature, sugar for inflection will be removed if it doesn't seem highly useful after trying it in a few significant projects. 

## PARTIAL RESULTS (2014 February)

I tried adverbs out for a while, but as they're designed above I don't favor them. A major issue seems to be that these modifiers need to be specific to a category - i.e. I want the same modifiers to mean different things based on whether I'm working with *lists* vs. *streams* vs. *processes* vs. *RDP behaviors*, and I want this *without context-sensitive ambiguity* - i.e. to be part of the word's definition.

For now, it seems better to just define the modified words by hand. It can cover most use-cases easily enough, and an IDE could presumably afford default implementations for modified words without requiring any smarts within the language itself.

For now, I'll return `\` to the pool of word characters. However, I'll reserve `-` for future applications.

