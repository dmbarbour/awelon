Note: No implementation of Awelon exists yet. But RDP have been adequately demonstrated with the Sirea project. Awelon itself is still in the design stages. 

Awelon
======

Awelon is a general purpose programming language based on reactive demand programming (RDP). RDP is a highly declarative, bidirectional dataflow model suitable for orchestration in open, distributed systems. To RDP, Awelon will add a rich type system, effects systems, external state models, a module system, and a distribution model. In addition, Awelon supports ad-hoc syntactic abstraction per module.

Awelon is aimed towards real-time applications and cyber-physical systems - user interfaces, multimedia, sensor networks, robotics, control systems. While any application may be expressed in Awelon, tasks that take an unbounded amount of time must often be modeled as incremental processing of intermediate state, which may be awkward (though does have advantages wrgt fairness, persistence, and process control).

Due to syntactic abstraction, Awelon doesn't have a fixed front-end syntax. Instead, awelon is primarily defined by a typed intermediate application language (intended for distribution), a machine description language (i.e. a set of typed resources and primitives for the target app), and a common module system. See the /doc directory for more information.

Getting Started
===============

Examples
--------




