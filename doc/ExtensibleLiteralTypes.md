
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

* canvases; direct image editing
* knobs and sliders and toggles
* color pickers
* modifying a state machine as a diagram
* manipulate views of a 3D mesh literals
* play sounds when mousing over a music literal

I think it's unreasonable to assume I could come up with any finite list of literals that is truly complete, and I wouldn't want to burden AO with a big list of literals anyway. But I might be able to develop a simple API for creating and interacting with ad-hoc literal types, and use either extension or convention to access this API. 

AO also compiles to Awelon Bytecode, which tends to preserve legibility for text and numbers. It would be useful if bytecode layer software components could directly integrate these much richer literals that can be extracted, viewed, and edited directly.

## Primary Design Candidate: Embedded Literal Objects

In AO and ABC, blocks are the only values that can meaningfully receive 'interactive' inputs. Blocks have a well-defined syntactic boundary, and are finite in nature. Thus, blocks are easily copied, pasted, and shared like more conventional literals. In general, a block can act like an object from object oriented programming languages. This is achievable by use of an `(method * parameters)` argument to the block, where the `method` might be a simple string like `"render"` or `"onTouch"`. We can include methods and logic to render and interactively update this object. We might also include self-documentation. 

The challenge, then, is three fold.

1. recognize a block as representing an ad-hoc embedded literal object
2. develop conventional set of methods and queries, portable to other IDEs
3. provide a simple means to develop and integrate new literal objects

To recognize such a block in AO, the simplest option is a new syntactic form. In ABC, we might instead apply an annotation to a block. Candidate representations:

        〚raw awelon bytecode here〛          in AO    
            (compiles to)
        [raw awelon bytecode here]{&O}l     in ABC

Here I'm proposing `〚` and `〛` (U+301A, U+301B) and raw ABC within the AO code. The use of raw ABC is useful because the literal object will be frequently rendered or updated under its own logic, and because it acts more like a text or number literal - independent of updates to the dictionary. The resulting ABC code uses a plain old block and indicates it as representing a literal via simple annotation. Developers can easily extract literals from bytecode back into AO. 

Developing a conventional set of methods and queries is a greater challenge. I imagine that any fixed set of methods will prove incomplete for some use case. But we could at least include some higher level conventions:

* cleanly divide query and update methods, e.g. `(query+update)`
* common query method to obtain self-documenting menu of methods

Clean separation of queries from update is useful because we rarely want to mix the two (e.g. viewing a literal should not modify it), and further because we must very clearly recognize when the output from a method should be rewritten back into the source code (thereby updating the embedded literal object). Having at least one common query for self description and introspection would enable development environments to offer object-dependent commands to a human user.

Assuming a preference for semantic, model-specific methods (e.g. `"addNewState"` for a state machine literal) and a clean separation of model and view/control, we'll need a little extra logic to interact with these embedded literal objects across a variety of development environments. 

A development environment would be designed to recognize a few popular views and interactive modes, e.g. view as text or SVG, along with interactive views with a 2D GUI widget. An object can include a few presentation methods that support these modes, e.g. presenting a button then translating it to an operation on the model. Whenever the literal object receives an update command, it will be rewritten in place and we'll regenerate all views based on querying the new model. Facebook's 'React' framework offers some hints on how to make this behavior efficient and avoid disrupting the user.

In practice, this allows us to create many new problem-specific literals and rich user interactions without updating the development environments. Updates to the development environments and adding new view/control models would be a rare event for supporting alternative HCI devices (e.g. multi-touch screens, Oculus VR goggles, Thalmic's Myo, LEAP motion) or new features (e.g. animations, video, sound, hardware acceleration). Or fixing bugs, of course.

Logics for presentation, update, query, documentation, etc. quickly add up.

Obviously, our embedded literal objects would grow excessively bloated without some other consideration. Fortunately, with liberal use of ABC resources (ABC's separate compilation and dynamic linking model) we can easily cache most of the logic and amortize costs across hundreds or thousands of literal object instances. ABC resource IDs can serve a similar reuse role as OOP classes:

        〚bytecode for instance state {#resource ID for fixpoint object logic}〛

Ultimately, for common object types, the transport and storage costs would be scarcely more than the representation of the instance state. And even that could be largely separated into resources, for very large objects.

My main concern for this design candidate: how shall we update existing literals? E.g. if we decide to extend image canvas literals with several new methods, what happens to the existing canvas literals? 

I think the 'right' answer here is that most literal object types should include some suitable import/export logics, such that we can replace the literal with a new version by importing data from the older version. But if we forget to include these logics, we can always resort to more ad-hoc mechanisms, e.g. rewriting some bytecode.

## Requirements, Desiderata, and Synthesis

Requirements:

* literals add one value to the stack
* literals are fully computable at compile-time
* computation of a literal should be pure
* meaning of a literal is independent of context
* can manipulate literal programmatically, too

Desiderata:

* simple in specification and nature
* support for interactive manipulation of literals
* extract and manipulate literals from ABC stream
* extensible command set, views, and tooling
* literal size independent of history of edits
* consistent across Awelon project environments

Synthesis:

* to receive inputs for interaction, literals use functions/objects/blocks
* blocks add one value to stack and are subject to easy partial evaluation
* easy to enforce purity by limiting embedded caps or evaluation environment
* development time quoting, binding, and computing leads to ad-hoc raw ABC
* raw ABC block maintains meaning across contexts and even in ABC streams
* can extract an arbitrary value from block by applying a query to it
* need conventions for consistent display across development environments
* couple presentation models for consistent distribution of new types

A single block as a literal is simple in specification and nature. Using an annotation to distinguish these blocks for display or manipulation even in an ABC stream or software component is simple in specification and nature. The candidate design, embedded literal objects, is the best I've developed to meet my goals for extensible literal types in the context of Awelon project.

An Awelon project development environment would need to recognize generic presentation models (e.g. text, SVG, GUI widget models, animated view modes), and might gradually be extended with new ones, but would generally not know anything specific about the meanings of literals, e.g. about music notation vs. state machine diagrams. 

Liberal use of ABC resources enables megabytes of presentation, query, and update logic to be represented and distributed efficiently, so long as that cost is amortized across enough instances of a datatype. There is no reason an image canvas couldn't include complete logic for a Photoshop-like manipulations toolset.

It's interesting to contrast this with the Type Specific Languages of Wyvern (from Jonathon Aldrich) or more conventional EDSLs. In some ways, AO's extensible literal model is essentially the inversion of such models, essentially operating on precompiled objects and live widgets embedded into the AO/ABC code, followed by ad-hoc programmatic manipulation of the literal and queries to extract values.

## Meta Thoughts

* An interesting possibility is that the same 'embedded literal object' model could also serve as a cornerstone for UI development in Awelon project. This would at least lend itself to the unification of programming and user experience.

* Embedded literal objects aren't refactored the same way as other code. Even just counting instance state, these objects could grow very large. I wonder if this will fill a useful niche for accumulating data within what would otherwise be an almost pure AO dictionary.
