
The type system for Awelon happens to be a common element connecting all the pieces. The Awelon virtual machines will be described using this type system. The AOs will be checked in this system. Expressiveness is ultimately determined by this system.

Q1: Effects Handlers vs. Capabilities? Or use both?
A1: Consider use of effects handlers (~ named, typed holes) as a partial alternative to capabilities and arguments. For encapsulation, I also need to support some 'opaque' effects that operate on an interface without knowing the details, but perhaps that can be modeled in some limited manner with dynamic behaviors?

Common Types
============

* **asynchronous product** (x & y) both are available at the same times.
    * x ~> x & x
    * x & y ~> x
* **choice of provider** (x | y) at most one is available at a time.
    * Either x y ~> x | y

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
