Here are a few idioms to declare types in Awelon project.

## Void Operations

We can distribute a value across void, then operate on the false version:

        a * e → a * ((1+0) * e) → ((a*1 + a*0) * e) → 
          ((a*0 + a*1) * e) → [(a*?)→y]*((a*0 + a*1)*e) → (y+a*1)*e →
          (a*1)*e → a * e

Any errors detected in this 'false' operation `[(a*?)→y]` can be understood as type errors. Thus, false operations can serve a similar role to type declarations. However, one cannot readily declare (in this design) that certain types should be polymorphic.

## Annotations useful for Typechecking

We can use some annotations. Presumably, we could have annotations of the form:

        {&type} :: (TypeDesc`a * (a * e)) → (TypeDesc`a * (a * e))
          ^ this is not recommended

But I'm not very fond of this approach because it requires explicit maintenance of the type description, and because the type description language would become too much of a baked-in standard. However, there may be other annotations that are useful and simple. One such annotation is:

        {&≡} :: (a*(a*e)) → (a*(a*e)); assert equality on `a` values

Asserting equality of values can be useful, for example when dealing with types parameterized by functions. Examples of such types include a map with a user-provided key comparison function. A small set of useful, simple annotations could possibly go far for type declarations. 

## Naming Conventions

Awelon Object (AO) language makes heavy use of naming conventions, e.g. for automatic testing and documentation. Adding a naming convention like `type.` or `typeOf.` would not be a stretch. Perhaps we could use `typeOf.word` to implicitly declare a type for a specific word (in a manner the IDE can track), while `type.` might operate on an arbitrary block (and be considered a form of testing).

Again, this introduces an issue of managing type descriptions. However, since this is at the Awelon Object layer, and not part of the normal runtime for words, there are far fewer issues with ad-hoc type descriptors that can be upgraded as the IDE improves.

At the moment, I don't have a good idea exactly what kind of type descriptors I want. But the following criteria are useful guides:

* types should be compositional, i.e. such that the type of a composition is a composition of types
* types should support polymorphism for specific inputs to a word
* should support dependent types
* should support some type inference
* ability to refine the domain for a word by use of types

I wonder, actually, whether approaching this as a describing a proof strategy, similar to theorem provers, rather than conventional type declarations.

Other interesting features would be some sort of quickcheck-like feature, e.g. such that types can guide construction and analysis of values, and support for ad-hoc property tests. With this criteria, a useful model for types might be a grammar, or somehow related to grammars.


## Multiple Type Systems?

One of the difficulties I've had with 'typechecking' is the whole issue of: what should be the 'official' type system? I guess that I've been *assuming* that I should only have one type system per AO installation (or something like that). 

This assumption is something of an intellectual trap, based on my long history working with PLs that each have just one type system. But having a single 'official' type system actually creates its own challenges, e.g. with respect to standards management and growth.

Let's consider the alternative.

Our dictionary can feasibly support multiple type systems, using naming conventions. Every type system may then be used to compute pass/fail/undecided (along with specific warnings or notices) on every word in the dictionary. An undecided option might also be the result of a timeout.

There may be a little support for type declarations, i.e. such that developers can insist that a value is polymorphic in certain ways. These mechanisms should be common to all type systems, such that all of them can leverage type declarations in a simple way. (This suggests an API for type systems against which type declarations are described.)

