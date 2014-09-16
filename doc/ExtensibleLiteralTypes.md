
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
        [raw awelon bytecode here]{&E}l     in ABC

Here I'm proposing `〚` and `〛` (U+301A, U+301B) and raw ABC within the AO code. The use of raw ABC is useful because the literal object will be frequently rendered or updated under its own logic, and because it acts more like a text or number literal - independent of updates to the dictionary. The resulting ABC code uses a plain old block and indicates it as representing a literal via simple annotation - here `E`, a rather arbitrary but sufficient candidate. Developers can easily extract literals from bytecode back into AO. 

Developing a conventional set of methods and queries is a greater challenge. I imagine that any fixed set of methods will prove incomplete for some use case. But we could at least include some higher level conventions:

* cleanly divide query and update methods, e.g. `(query+update)`
* common query method to obtain self-documenting menu of methods

Clean separation of queries from update is useful because we rarely want to mix the two (e.g. viewing a literal should not modify it), and further because we must very clearly recognize when the output from a method should be rewritten back into the source code (thereby updating the embedded literal object). Having at least one common query for self description and introspection would enable development environments to offer object-dependent commands to a human user.

A reasonable concern is whether updates and queries should be *semantic* or *generic* in nature. E.g. a semantic update for a state machine diagram might include methods like `"addNewState"`, whereas a generic approach might include methodds like `"onMouseClick"`. I would favor semantic methods. Separation of a semantic model from an environment-specific view and control is valuable for a number of reasons that I shouldn't need to mention here.

Assuming a preference for semantic methods, we'll need an extra layer of indirection to interact with these embedded literal objects across a variety of development environments. Objects would include a variety of methods, both for non-interactive presentation (e.g. as text or SVG) and interactive manipulation (e.g. as a UI widget or VR object). (Alternatively, we could push the presentation logics into the environments, e.g. recognize a state machine diagram by some mime-type descriptor and select an appropriate plugin for presentation. However, including the logic within each object is a better fit for Awelon's code-as-material metaphor, and is better for distributing new types and specializations.)

Including a large amount of logic for update, presentation, introspection, documentation, etc. isn't a big concern, at least not if we assume liberal use of ABC resources. The logic costs for a new type of literal might be amortized across hundreds or thousands of instances. 

The main concern for this candidate is: how do we update existing literals? E.g. if we decide to extend image canvas literals with several new methods, what happens to the existing canvas literals? It turns out there are a number of ad-hoc approaches (e.g. rewriting bytecode). But one of the better options might be to provide some methods with the expectation that we'll eventually want to upgrade, e.g. import/export methods for data. We can also use wrappers or adapter patterns, if necessary. 

## Design Requirements and Desiderata

Requirements:

* literals add one value to the stack
* literals are fully computable at compile-time
* computation of a literal should be pure
* meaning of a literal is independent of context

Desiderata:

* simple in specification and nature
* support for interactive manipulation of literals
* extract and manipulate literals from ABC stream
* extensible command set, views, and tooling
* literal size independent of history of edits
* consistent across Awelon project environments

I believe the candidate effectively meets both requirements and desiderata. 
