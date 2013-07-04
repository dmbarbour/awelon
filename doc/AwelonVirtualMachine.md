
Application, Not Machine
========================

The real requirement is to define what primitives and parameters are available to an application. In a sense, this is an environment that contains even the lowest level primitives (e.g. adding two integers); there are no implicit primitives. But in another sense, this represents the 'machine' primitives, i.e. the machine code. Using partitions, this code can be heterogeneous. 

One relevant concern is how to handle implicit vs. explicit authority, in particular the distribution thereof to dynamic behaviors. I would like to ensure capability security, which suggests that the implicit capabilities must be those that grant no authority and instead represent calculation features. But it may be feasible to treat all as capabilities, assuming the low level primitives can be packaged conveniently. 



The Heterogeneous, Partitioned, Distributed Machine
===================================================

Today's computation technologies are very heterogeneous, with CPUs, GPUs, FPGAs. These different technologies may support different type primitives (e.g. integers of limited sizes, floats or doubles), and have access to different effects or capabilities. For security reasons, access to effects may be further constrained or occur at higher abstraction levels, contributing to the variation. There is no ad-hoc connectivity. For example, to program the GPU, one must first get a program to an associated CPU. Thus, there are various 'reachability' concerns that must be addressed.

Over time, the available technologies and targets will change and continue to grow. Awelon must be able to grow with it. 

Machine Description Language
============================

Awelon supports a virtual machine description language, which is really just a bunch of symbols with types and other documentation or metadata. This is the sort of information you might find in a language standards appendix.

* **behaviors** - declare axiomatic primitive behaviors, i.e. behaviors that are assumed to exist without any definition. Each behavior is declared with its type. Behaviors may have simple static parameters. Documentation, deprecation, and other ad-hoc attributes may be associated with a behavior.
* **signal types** - declare a set of primitive signal types. Types may have simple static parameters. Documentation and other ad-hoc attributes may be associated with a type, i.e. to describe its purpose.
* **partitions** - declare a set of partitions which may have simple static parameters. Documentation and other ad-hoc attributes may be associated with a partition. Heterogeneity is expressed largely by partitioning and having behaviors that are available operate within specific partitions. Staged computation can be expressed by controlling connectivity between partitions.
* **application models** - declare a named application model, or **target**. This is a behavior type, e.g. the type for a `main` behavior, or for widgets, servelets, plugins, or whatever is appropriate. Targets may have simple static parameters of known types. Documentation and other ad-hoc attributes may be associated with a target. Targets aren't really part of the virtual machine but are convenient for consistent documentation and named parameters. An application object (AO) will target a declared application model by name, and will be parameterized by its user. 
* **equivalence laws** - declare equivalence laws for composite behaviors or types.

Names are not overloaded. If a name is used for a type, it cannot also be used for a partition, behavior, target, or equvialence rule. This is intended for clarity of error messages.

The Awelon virtual machine (AVM) description is represented in a string, typically stored with a .avm extension. Defining new AVMs is rare, the domain of Internet visionaries, standards committees, language designers, and OS architects rather than normal developers. Backend compilers will often be AVM-specific, potentially even target specific, so it may be difficult to distribute a new AVM. AVMs are primarily for documentation and integration purposes. Though, use of ad-hoc metadata and attributes may support an automated implementation. 

To support a little bit of modularity, AVMs can be composed by a simplistic form of inheritance: each AVM may name zero or more other AVMs from which it inherits, along with the names it inherits, with potential renaming (e.g. foo as bar). The inherited names become part of the resulting AVM. Since the AVM doesn't actually define applications or values, there is no need for complicated import/export semantics. 

No Assumptions
==============

Other than the behaviors and types explicitly declared as primitive in the AVM file, there are no runtime primitives. Awelon does not require any universal assumptions beyond the type system's own computations. However, Awelon does need a common (assumed) type description language 

Capabilities vs Ambient Authority
=================================

Ambient authority is authority based on the context in which code is executed - e.g. which partition, who is running the process. Sadly, ambient authority is not very expressive or precise for security policies, and is difficult to control (since it's invisible). A preferable design is object capability model, where authority is granted through explicit parameter and threaded explicitly through an application. There is also an intermediate form of authority, e.g. where a key or certificate is threaded through a program (with an intermediate level of expressiveness; keys do not easily model attenuation).

AVM can express any of these possibilities. If resources are made available as toplevel behaviors, you'll have ambient authority. If resources are provided as parameters to the target application model, then you'll have capability security. Sealer/unsealer pairs (which themselves are resources) can model the intermediate option.

For Awelon, I'm not entirely sure which will be favored. Ambient authority has its advantages, for convenience, and a reasonable level of security can likely be achieved via heterogeneous partitioning (e.g. secure vs. insecure partitions). Awelon's module system is relatively *flat*, which hinders deep cross-module uses of parameters (except indirectly, via dynamic behaviors).

