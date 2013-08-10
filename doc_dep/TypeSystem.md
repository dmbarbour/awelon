
The type system for Awelon happens to be a common element connecting all the pieces. The Awelon virtual machines will be described using this type system. The AOs will be checked in this system. Expressiveness is ultimately determined by this system.

Requirements?
=============

1. Easy to serialize the types for remote systems.
2. Easy to typecheck awelon virtual machine code (i.e. the code itself is a relatively trivial proof).
3. Effective support for object capability security, i.e. where the code is hidden.
4. Effective support for heterogeneous partitions, e.g. client vs. server, CPU vs. GPU.
5. Effective support for 'generic' partitions (i.e. forall a . Client a).
6. Every value, e.g. every integer, has an associated temporal-spatial attributes. 
7. Effective support for linear types, i.e. no default drop or default dup.
8. Effective support for use-by and expiration types (a temporal aspect of linear types).
9. Effective support for pipeline computations, i.e. multiple heterogeneous stages in one direction where we can't escape the pipeline. 

Desiderata
----------

1. Effective support for 'strategies', i.e. context-dependent code where the context depends on a desired output type.
2. Effective support for fan-in and fan-out control on specific or generic resources. 
3. Effective support for controlling loops, perhaps by partition-separation of resources. 
4. Complete model of types in AVM form. I.e. even the primitives are represented explicitly, so there are no default types. 

Some of my goals would benefit from runtime access to types, i.e. so we can use a constraint model to build and compose code. So some sort of type-level reflection is also a desiderat, though I'm not sure how to best go about that. 

Rather than strategies, some specification of equivalences in the AVM might also work.


Modeling Capabilities
=====================

Capability security requires hiding the implementation details (e.g. which resources are accessed) by dynamic behaviors. If I don't have algebraic effects or similar, this should be trivial: dynamic behaviors will always be capability secure by the type system. 

If I do allow algebraic effects or similar, I will need to address this explicitly. Something like:

        register :: (a in p) => (S p (a ~> b)) ~> (S p (Cap p (a ~> b))
        invoke :: (S p (Cap p (a ~> b)) :&: a ~> b

Or just:
   
        tocap :: (a in p, HasCapabilities p) => (S p (a ~> b)) ~> (S p (a ~> b))

But currently I favor simple avoiding the ability to decompose behaviors, i.e. forbid algebraic effects or free interpretation. 

Thoughts on Theorem Provers
============================

Coq may be an effective language for modeling the Awelon type system, virtual machines, and basic tactics. I wonder if representing the AVM in Coq - or perhaps a simple subset of Coq that can compile to Coq - would be a good design choice. Coq could also be used for 'proof extraction' as an approach to compilation. I think Coq won't be much good for tracking resources, though, nor for staged computations (e.g. module-layer computations). 

I think it might work well up to a single AO. I foresee some difficulty with this approach for modeling staged, cross-module computations - e.g. link-time composition and integration. But it could work well enough for simple apps and validation. 

The most important use of theorem provers might be to help specify an initial, usable type system.


AVM Dependent Types
===================

The types available for a particular application will depend on the Awelon Virtual Machine and partition. 


Notes
=====

The type system is the real heart of Awelon language. We don't have a fixed set of primitives, nor a fixed syntax, but we do need a common type model so we can validate distributd app code against a target machine description.

Requirements
============

It is necessary to target and interact with existing devices. Therefore, such type models as simple integers, 

An RDP model distributes and manipulates values. For simplicity


Type Language
=============




Richly Typed
============
