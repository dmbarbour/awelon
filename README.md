
Awelon
======

Awelon is a safe, low-level language for reactive demand programming (RDP, see the [primer](doc/RDP_Primer.md)). RDP is a reactive dataflow model developed for handling open distributed systems. Awelon is intended to become the primary distribution language for RDP code and components. Awelon has a variety of features orthogonal to its RDP elements (type-driven behavior, static signals, etc).

An Awelon system consists primarily of three elements:

* the Awelon Virtual Machine (AVM) definition file, which documents the heterogeneous distributed machine
* the Awelon Object (AO) files, which describe RDP behaviors representing applications or reusable components
* a set of compilers and optimizers, which operate on AOs and ultimately generate executable code

More details below.
Awelon Virtual Machine
======================


An AVM describes the primitive behaviors and their types, equational laws, proposed rewrite rules, and standard tactics. The AVM may also contain useful English descriptors for each primitive. In practice, there will be very few AVM files, and they will tend to evolve slowly after reaching maturity. The AVM is the only location that Awelon's type language is used explicitly.

The AO is the basic unit of Awelon code. Each AO describes a *set* of potential RDP behaviors. From that set, one behavior will be statically selected. The decision is type-driven, sensitive to both the required input and output types, enabling Awelon code to be adaptive and context sensitive, and potentially more reusable. The set of behaviors is represented as a weighted grammar that can generate multiple behaviors of different types. 

Every AO is *self-contained* and *complete* - no holes to fill, no external linker. These properties are valuable for code distribution, and for sharing or distribution of reusable component-oriented software. That said, the traditional role of parameters can be achieved by use of 'static' signals. Also, linking can be modeled internally, i.e. in terms of actively searching a link-time environment (or publishing resources to it) in a staged application model.

Compilers or optimizers may be specialized to an AVM or application type. Initial efforts will target rich web application servers... then cloud, mobile, or gaming. This Awelon project in Haskell defines some of these initial compilers. 




Awelon Objects
==============
 

http://en.wikipedia.org/wiki/Tacit_programming

 (e.g. type safety, linear types, dependent types, [negative and fractional types](http://www.cs.indiana.edu/~rpjames/papers/rational.subc.pdf), tacit programming). 


RDP does not need traditional loops; recursion patterns are better addressed by 'foreach' operations 






Structure of AO
===============

An AO is a relatively compact, finite string of unicode characters in the style of J or APL. White space is ignored. It is not very human readable, rather is intended to be easily transmitted or processed by machines. An IDE may be able to display the AO code in a graphical manner for useful editing or debugging.

The basic concept of RDP is that we process time-varying signals.



The AVM associated with an AO string is generally implicit when the string is transmitted. However, it would be wise to specify the AVM version using a dedicated behavior (defined in the the AVM). 


An AO is represented in a unicode string or UTF-8 file (extension **.ao**). As a unit of distribution, an AO must be pretty much self-contained. That is, there are no implicit link-time dependencies because the recipient might not have access to them. (However, there may be more explicit forms of linking that can be encoded in the RDP behavior.)

i.e. having no link dependencies.  

(*) Awelon does not provide any default mechanism for linking. However, a particular AVM might provide behaviors suitable for compile-time or runtime linking.

The structure of an AO is very simple: a reference to a parent resource (described below), a set of local definitions, and the exported main behavior. The local definitions are with simple substitution into the main behavior.





A single AO represents a *set* of possible RDP behaviors, from which one behavior will be selected based on static type. 

 formally modeled as a tactic for selecting one behavior of that set. The actual behavior associated with an AO 

a *set* of possible RDP behaviors, from which one behavior will be selected based on static type. (That is, an AO is not a library of behaviors; it is just one RDP behavior that happens to be adaptive to its usage context.)

 is desired (for both input and output). 

 based on usage context 

This code will represent an RDP behavior, or a set of behaviors selected by static type. A single AO may describe behaviors that cross multiple heterogenous 'partitons' - as between client and server, or CPU and GPU. Each partition may potentially have its own set of primitive behaviors, types, and resources. 

An AO may serve as an application or a reusable component. An AO cannot directly depend upon other AOs

 different kinds of applications depending on the starting partition and input types. 

 each partition may have its own resources.

  as between client and server, or CPU and GPU. 

A single Awelon Object (AO) - a component, represented in a file or string - may describe behaviors that are distributed across multiple components.


An AO is a complete component, there is no implicit notion of external dependencies (though a particula

An AO will be further compiled - i.e. to a mix of JavaScript and machine code - to execute on available architectures. 

Awelon 

An Awelon Object (AO) can serve as a component in a component-based system. An Awelon *application* is simply a component that has the right type to operate 


Awelon is not intended directly for human use, though that usage is not prevented. 

 must be further compiled, e.g. to machine code, or LLVM, or JavaScript, or a mix of these in order to execute on modern systems. However, an AO



RDP is a reactive, declarative dataflow programming model that allows access to effects. RDP is designed to support large scale distributed systems and heterogeneous computation environments. a single Awelon object can describe both server and client-DOM behavior, or include code for both CPU and GPU.



 kinds of code for GPU vs. CPU. 

, and is carefully designed to support large scale distributed systems... and potentially *heterogeneous* systems (e.g. integrating GPU, FPGA, HTML client, server side, etc. code into one object)

  declarative, reactive, dataflow programming model 

suitable for heterogeneous systems (e.g. defining client and server code in one program), distributed systems, open systems. 

 with constrained effects (in particular, effects are commutative and idempotent). 

Traditionally, low-level languages are procedural or imperative, close in nature to CPUs. 

The role of Awelon is the specification of safe, distributable "Awelon Objects" that can l

Awelon is a general purpose programming language based on reactive demand programming (RDP). RDP is a highly declarative, bidirectional dataflow model suitable for orchestration in open, distributed systems. To RDP, Awelon will add a rich type system, effects systems, external state models, a module system, and a distribution model. In addition, Awelon supports ad-hoc syntactic abstraction per module.

Awelon is aimed towards real-time applications and cyber-physical systems - user interfaces, multimedia, sensor networks, robotics, control systems. While any application may be expressed in Awelon, tasks that take an unbounded amount of time must often be modeled as incremental processing of intermediate state, which may be awkward (though does have advantages wrgt fairness, persistence, and process control).

Due to syntactic abstraction, Awelon doesn't have a fixed front-end syntax. Instead, awelon is primarily defined by a typed intermediate application language (intended for distribution), a machine description language (i.e. a set of typed resources and primitives for the target app), and a common module system. See the /doc directory for more information.

Getting Started
===============

Examples
--------




