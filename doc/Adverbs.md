*Regarding 'Adverbs'*

In word-based functional concatenative languages, it isn't difficult to express that a word is mapped over a sequence. Some expression of the form:
 
        [ foo ] each

should be sufficient. Unfortunately, use of brackets for quotation becomes surprisingly noisy and difficult to read in practice, i.e. once we have sentences of more than a few words.

J and K languages have an interesting concept of 'adverbs', which modify how a word is applied without explicitly reifying the word. This is interesting because, applied to languages like Factor or AO, it could greatly reduce noise and improve parsimony. I.e. we can take this "best part of" J and K and steal it for concatenative languages.

To realize the same concept in a word-based PL implies interpreting a limited sub-structure of each word - e.g. the suffix. We might imagine that:

        foo⋆

should have the same behavior as `[ foo ] each`, but without any requirement to define `foo⋆` explicitly. This technique should be composable, such that `foo⋆⋆` has the behavior of `[ [ foo ] each ] each` - thereby operating on each element of a matrix. 

We could further formalize this behavior, and support user-defined adverbs, while sticking aggressively to the concatenative philosophy. For example, we might systematically rewrite `foo⋆⋆` as: 

        [ foo ] \⋆ \⋆ inline

Though... this particular semantics is less-than-ideal because it allows adverbs access to the full stack. The type for an adverb should be  `(x→y)→(x'→y')` - i.e. a context-free function from verb to verb. So the actual rewrite might be a little bit more involved.

At least one important requirement remains to be addressed:  I need a systematic way to parse `foo⋆⋆` into a word with two adverbs! Why not `[ foo ] \⋆⋆ inline` or `[ f ] \o \o \⋆ \⋆ inline`? 

The obvious answer, I think, is to reserve a few characters and introduce some disambiguating syntactic structure for parsing words. For example, I could use a backslash like `foo\⋆\⋆`. But, IMO, that is aesthetically unpleasing and risks aggregating the s the same syntactic noise I'm hoping to avoid in the first place. 

Can we do better?

In a few conversations regarding adverbs, my strong impression was that we don't need many of them - that just a few can go a very long way. If this is true, then perhaps we could restrict to one character per adverb. That restriction appeals both to parsimony and my aesthetic sense. So we could have `foo\⋆⋆` - not too bad.

Perhaps, then, we could make adverbs separable from their use, i.e. such that `\⋆⋆` can be understood separately from the 'foo'. This will make it easier to refactor code using adverbs. Also, rewriting then becomes much more incremental.

        foo\⋆⋆ = [ foo ] \⋆⋆ inline = [ foo ] \⋆ \⋆ inline

Potential roles for adverbs include:

* collection-oriented operations (map, fold)
* minor data shuffling - e.g. flipped arguments
* quotation of a single word
* partial application and currying
* deep application - dip, zippers
* exponentiation, bi, tri, etc.


