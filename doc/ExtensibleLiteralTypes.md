
AO supports literal numbers and text

        42
        3e9
          "Hello, world!"
        "multi line 
         text
        ~

But I can imagine applications for:

* music staff
* graphs 
* decision trees
* matrices and vectors
* diagrams and images
* colors
* 3D meshes

Further, I love the idea of *interactive* literals such as:

* canvases
* knobs and sliders
* toggles
* color pickers
* shift views of a 3D mesh literal
* play sounds when mousing over a music literal

I think it's unreasonable to assume I could come up with any finite list of literals that is truly complete, and I wouldn't want to burden AO with a big list of literals anyway. But I might be able to develop a simple API for creating and interacting with ad-hoc literal types, and use either extension or convention to access this API. 

## Requirements and Desiderata

Requirements:

* literals add one value to the stack
* literals are fully computable at compile-time
* computation of a literal should be pure
* meaning of a literal is independent of context

Desiderata:

* support for interactive manipulation of literals
* extract and manipulate literals from ABC stream
* extensible command set, views, and tooling
* literal size commensurate with final output
* consistent across IDEs and Awelon project languages

The requirements seem easy to meet. AO and ABC are already capability languages, and are readily able to confine literal construction to a simple environment. The desiderata seem more challenging to meet, and may prove contradictory.

Analysis:

* To interact with literals at the AO layer:
  * literal is capable of receiving inputs
  * result of processing input is updated literal
  * edit-time computation for update and display
  * hence edit-time interpretation or compilation
* To extract and manipulate literals at the ABC layer:
  * we must uniformly indicate literals in the ABC stream
  * the literal must have a clear boundaries generated ABC
  * literals must contain logic for views and updates
  * literals must be self-describing; views, commands, etc.
  * update logic must not depend upon AO dictionary
* To support extensible command sets, views, and tooling:
  * commands composable and programmable - paintbrushes, tools
  * may rely much on convention
  * upgrade paths by wrapping or export/import metaphors
* For literal size commensurate with final output:
  * literal must discard and reduce irrelevant inputs
  * history of literal edits is not locally preserved
  * type-specific logic refactors easily to ABC resources
  * popular refined types eventually supported via ABCD 
* Consistency across IDEs and Awelon project languages
  * easy export and import via intermediate ABC
  * literal values cannot depend upon AO dictionary 
  * a simple, common type with predictable behavior
  * may reduce value extraction to standard ABCD primitive
  * consistent conventions per Awelon project language

## Primary Design Candidate: Embedded Literal Objects

In AO and ABC, blocks are the only values that can meaningfully 'receive inputs'. Blocks also have a well-defined boundary. By clever use of fixpoint functions, each block can generate a fresh block as the next literal. In this sense, literals might appear as simple process or object models:

        µP.[cmd→(P*result)]

Of course, we need common types and conventions to:

* render the literal to a canvas (self-render?)
* obtain a menu of commands and documentation
* systematically distinguish updates from queries

        queries in left     (query+0)
        updates in right    (0+update)
        dependent type: µP.[query → result | update → P]

But it may be worth leaving these decisions to convention. 

Assuming we handle all that, what are the implications of a block model?

1. Such a block must exist at the ABC layer. It will not contain AO words. This is useful for various desiderata and requirements:
  * interaction and updates will diverge from any dictionary
  * literals extracted from ABC don't even start from dictionary
  * context-independence in same sense as text and numbers
  * independent of any language within Awelon project
2. We must embed this ABC block within the higher level language that aims to support extensible program types. For AO, it can be awkward to embed raw ABC, e.g. `#42` becomes `%v 42 %c`. 
3. The IDE must easily recognize the ABC block representing an embedded literal. Again, this seems awkward in AO as it stands. In the ABC layer, we might use a simple annotation, e.g. `[block]{&literal}` to specify the preceding block for display as a literal, then proceed to extract the literal value. But in AO, this corresponds to a more awkward `[block] %r {&literal}` or `%v [block] %c {&literal}`, which would not be consistent between IDEs.
4. The IDE must systematically update the literal, i.e. reading it from the code, processing it with the command, then embedding it back into the source code. At least logically. An IDE might handle these literals specially and serialize as needed.

Between the second and third forces, I believe I can make a strong case for dedicating a tiny slice of AO syntax (and that of future textual Awelon project languages) to embedding of literal objects, i.e. to reduce awkwardness and improve consistency. The proposal is to leverage unicode `〚` and `〛` (U+301A, U+301B):

        〚raw awelon bytecode here〛                  in AO
        [raw awelon bytecode here]{&literal}l       in ABC

Higher unicode codepoints are painful to type on most keyboards. But, in this case, the IDE would be doing the grunt work, so it shouldn't become a problem. No ABC extensions are necessary. I'll likely seek something shorter than `{&literal}` for this role, perhaps `{&o}` or `{&!}`.

I envision that an AO IDE might provide a menu of initial literal objects based on words in the dictionary like `literal.newMusicStaff` and `literal.newCanvas` (or perhaps a specific type for menu options, allowing extra description, documentation, and initial parameters). Selecting an item would compile that word down to ABC, and embed it between white square brackets. The literal would then be rendered on the IDE's canvas, perhaps staking out its own area, and would henceforth evolve independently of the original literal constructor.

This proposal has a few challenges remaining:

* lacks a clean separation of view from value
* need a consistent, common command set for IDEs
* conventions for extensible command set and upgrade
* conventions for mashup and composition of literals

We could have a standard command to 'evaluate' a literal, to extract some other value from it. But it doesn't seem essential, and keeping the embedded block-literal concept separate from the processing is a valuable idea. It keeps convention separate from the formal AO extension. We can combine raw literals, or perform context-dependent extractions if we really desire it.

## Meta

Literals are very problem specific, grow indefinitely, and resist most efforts at refactoring. The few refactorings we can perform are based on refining common structure into new literal types. I believe literals may serve a useful role, in counterpoint to AO words whose definitions cannot readily be more than 10-20 words. A picture can be worth a thousand words, more or less, without growing too unwieldy. No matter how sophisticated the literal, we only generate one value on the stack... and we can easily *see* the literal, i.e. asking it to render itself.

A lot of embedded DSLs might be best modeled as embedded literal objects.

These embedded literals assume a serializable stream of commands. Cooperative work will require further constraints on the nature of the literal.

Of course, after specifying that a block is a literal, we must typically extract its value. Keeping this as a separate step in AO can be useful, since we might want to keep literal objects as literal objects, e.g. to perform post-hoc manipulations and compositions.

## Deprecated Designs

Some of my earlier designs focused on naming conventions. Some also focused on generating a new block of AO code after each update. E.g. we might have: `[a block of AO code] literal.foo` and we'd implicitly seek words like `ui.literal.foo` for rendering and commands. 

This approach had an advantage of not embedding the entire UI model in the ABC, but that's also a disadvantage for viewing literals in the ABC stream or reusing them. In the new model, we must rely on partial evaluation to eliminate the UI context when deeply optimizing.
