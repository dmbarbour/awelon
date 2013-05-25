Note: No implementation of Awelon exists yet. But RDP have been adequately demonstrated with the Sirea project.

Awelon
======

Awelon is a general purpose programming language based on reactive demand programming (RDP). RDP is a highly declarative, bidirectional dataflow model suitable for orchestration in open, distributed systems. To RDP, Awelon adds support for dependent types, substructural types, sealers/unsealers, ad-hoc state, orthogonal persistence, effects handlers, specialized matrix/vector processing, type-driven wiring, distrbuted programming, an adaptable module system, and syntactic abstraction. 

Awelon is primarily suitable for real-time applications - user interfaces, multimedia, sensor networks, robotics, control systems. Any application may be expressed in Awelon, but tasks that take an unpredictable amount of time must often be modeled as incremental processing of intermediate state, which can be awkward (though does have advantages for fairness, persistence, and process control).

Getting Started
===============

Examples
--------


Awelon's Application Model
==========================

Awelon's application model is very different from the tradition developed for procedural languages. However, I believe this new application model will result in a generally superior programming experience.


Application Definition and the Module System
--------------------------------------------

An application in Awelon is a set of anonymous modules. Any module in the set may contribute to the runtime behavior of the application; there is no privileged 'main' module. It is possible to extend an Awelon application by adding a module. 

Contributions are achieved effectfully. An Awelon compiler provides a *link environment*, and modules act as effectful agents, manipulating the environment synchronously (at the same logical time, via RDP's semantics). In practice, many modules won't contribute directly to runtime behavior. The link environment has abundant resources, which developers may leverage for collaborative data structures or link-time metaprogramming. Knowledge, constraints, world-maps, dialog trees, UI specification, etc. may be scattered across modules then combined at link time by a few specific modules. It is easier to reuse modules that operate on peripheral resources. A few modules will contribute to specific resources that determine the runtime behavior.

The link environment must reach a stable state. In a stable state, the runtime behavior is constant and may be extracted. The environment is still *logically* present, e.g. with respect to dynamic behaviors that would otherwise expire, but the computation may be discarded. During development, the link environment may be kept available for browsing and debugging.

Awelon's application model supports a style I call [stone soup programming](http://awelonblue.wordpress.com/2012/09/12/stone-soup-programming/). Developers provide the ingredients for a 'soup' of modules, which meld in their link environment, ultimately generating a runtime behavior. In Awelon, the entire responsibility of each module is to define its own link-time behavior, manipulating the link environment. 

This design is not without its caveats. The effective *meaning* of a module is context dependent, difficult to understand or validate in isolation. Similarly, it can be difficult to isolate bugs to a particular module, especially if high levels of metaprogramming are involved. But I believe these issues can be addressed by a good development environment and mitigated by a little discipline (e.g. ensuring modules have just one clearly documented responsibility).

Compilation and Real World Integration
--------------------------------------

Compilation consists of parsing each module, allowing the set of modules to interact in a link environment, waiting for the link environment to stabilize. The stable runtime behavior is extracted from a standard location then serialized to an Awelon Object (the/an AO), which is later postprocessed (e.g. optimized). Awelon has a standard Awelon Object language (AO) used for this serialization. AO is a pure, strongly typed, proof-carrying language that defines a typed RDP behavior in context of a typed RDP-based API.

The generated AO may be interpreted or further compiled to imperative code. If the AO describes an end-user application assuming an API analogous to the HTML5 DOM, it might further be compiled to Wx and C++, or HTML and JavaScript. A compiler could be specialized for its API and target, or could be generic via pluggable runtimes.

Early backends for Awelon will target standalone end-user apps and real-time web-app servers. I may also pursue support for a popular low-level pubsub architecture (e.g. ROS or DDS) for flexible systems integration.

Awelon does not provide a foreign function interface (FFI). FFIs are deeply problematic with regards to safety, security, extension, distribution, optimization, portability, and GC. An imperative FFI would be also be an awkward fit for RDP.

Parsing Modules and User-Defined Languages
------------------------------------------

Every module identifies the interpreter with which the module text is to be parsed and processed. Within an Awelon source file, each module starts with `@foo` on a new line where `foo` is any symbol and identifies the language. Each parse occurs in a fresh *parse environment*. Like the link environment, the parse environment must stabilize and may afterwards be discarded. A parse can be cached by extracting the resulting link-time behavior to an AO. 

New user-defined languages are developed as separate applications. A language app will receive a string and generate a link-time behavior, and operates in a parse environment. The Awelon compiler will recognize languages provided as AOs. General purpose user-defined language can feasibly be bootstrapped, such that it is eventually maintained in itself.

Users have a great deal of freedom in language definition. Languages don't need to be general purpose. A user might define highly problem-specific languages, e.g. for world maps or dialog trees or UI widgets. It is possible to have tool-specific languages, e.g. to support easy correspondence between a graphical program editor and the text. But many languages will be small tweaks to address painful boiler-plate or utility definitions. The Awelon compiler will distribute with a suite of default languages, but developers don't need to use them. Developers have full ability to configure which language processors are used per application.

User-defined language is a primary mechanism for reusable abstractions in Awelon.

Abstractions typically come in two flavors: vocabulary extensions and abstract interfaces. In the former case, we want a specific meaning without writing it out by hand each time. In the latter case, we don't know specifically what we want; that's somebody else's problem. For vocabulary extension, Awelon developers don't have a very effective choice other than language manipulation. A language may provide local definitions, but those aren't reusable. Awelon has no built-in import semantics. We can potentially model import as a link-time behavior, but the boiler-plate involved with a big list of imports would still be a suitable target for a new language.


Annotation, Optimization, Postprocessing
----------------------------------------

An AO extracted from a link or parse can be subjected to ad-hoc optimizations. RDP has rich equational reasoning properties, and there are many optimizations that might be applied. But one might also develop specialized optimizations based on knowledge of the target API and resource model, or even more specialized knowledge of the usage context, or performance assumptions. Developers will have much freedom to implement their own optimizers.

Awelon provides support for annotation of behaviors and types, accessible via an AO. This allows developers or intermediate processes to encourage specific optimizations - e.g. to cache or memoize a particular value, or to specialize against a presumably stable signal. Annotations can also support debugging (e.g. traces, probes, assertions), metadata (e.g. authors, PKI signatures, licenses, source tracking), or could even be used for alternative composition models (e.g. aspect-oriented programming, defaults and overrides). But annotations are not a formal part of semantics, and are not directly observable from within Awelon. Annotations may be scrubbed in some cases to discourage accidental dependencies.

User-defined optimizers and post-processes aren't required to be behavior preserving, nor even type-preserving. Those concerns are left to the discretion and discipline of developers. But the resulting AO must describe a provably typesafe behavior. Like user-defined language, Awelon compilers only recognize postprocesses provided as AOs. 


Application Specification
-------------------------

A configured application (excluding the backends) primarily consists of:

* specified runtime API and behavior type
* language interpreters (override defaults)
* pipeline of postprocessors (if not default)
* the set of anonymous modules

In general, the first three are reusable across applications and are provided as AOs. The runtime API and behavior type are provided by a 'prototype' behavior AO. The Awelon platform will come with prototypes for the different kinds of backends it supports. Language interpreters are provided as a id:AO pairs. The postprocessing pipeline is simply a list of AOs. While anonymous modules cannot be named, they can be provided directly, or an external source or container of modules may be named.

Awelon's implmentation assumes configuration via the filesystem. AOs are named in the filesystem with a ".ao" extension, an Awelon project with ".ap", and an Awelon source file with ".aw". An Awelon project configures an application, centralizes its definition, ensures there are no hidden dependencies. An example configuration might look like:

        (myApp.ap)
        @prototype standaloneUI.ao
        @postproc optx.ao opty.ao optz.ao
        @language foo fooLang.ao 
        @language bar dir/path/bar.ao
        @source myApp.aw
        @source aux.aw

A ".aw" source file may contain any number of modules. Each module starts with `@` on a new line. (The space prior to the first module ignored.) The first word of each module identifies the language processor with which the rest is interpreted. I.e. `@foo` will specify use of the `foo` language. Everything past the first word to the next module or end of file is processed by the appropriate languge element. 

*Aside:* This configuration syntax is from Joe Armstrong of Erlang's ML9. The flat structure suitable for Awelon. However, Awelon does not generally support the Erlang record at the start of a section (though a particular language might).

### Inline Project Modules

The Awelon project supports inline modules.

        @module foo this is parsed in language identified as 'foo'
        and so is this, until next @ symbol on new line (or EOF)

The first word specifies the interpreter with which the rest of the module is parsed. Inline modules allow an entire application to be embedded in one big project file.

### Application Decomposition and Composition

Often it would be useful to decompose an application into components, which may be separately compiled then composed. The pools of modules for each component may be smaller, and the component types more specialized. I'm still thinking of how best to achieve this. Separating the app isn't the hard part; it's naming the pieces and stitching them together in the parent app that gets tough.

* environment extension is feasible (modeled as a post-process) but has the negative consequence of specializing modules to their application (i.e. because we have application-specific environments).
* the type system could also enforce unique fan-in on certain resources. This could achieve the same as environment extension at risk of compile-time conflicts between modules that target the same resource. While more flexible, this does hinder ad-hoc app extensibility, which I'd prefer to avoid.
* the component app could simply form another link-time stage.
* support hierarchical environments? no, I do want separate compilation.

In general, we cannot usefully compose applications just by tossing all their modules into one set. By analogy, that's like attempting a "clam beef chowder stew" just by putting the uncooked ingredients in one pot - it might work, it might not, but it's quite an experiment and difficult to reason about. A valid alternative is to build multiple apps separately then compose them (or, conversely, decompose a big app into smaller ones).


Live Programming and Debugging
------------------------------

The risk of this design is that runtime behavior becomes context-dependent. Modules may be difficult to debug individually. And with high levels of metaprogramming, behavior also becomes very indirect: there is nothing directly in code to trace. These concerns can be mitigated. With a good development environment, developers should be able to observe and debug the compile-time environment directly, and even access resources that serve as the RDP-equivalent of error logs. 


The parse environment provides few convenience methods, a place to report warnings, features for interweaving eventual live code feedback, support for syntax highlighting, etc.. 
To support live coding, 

developers may have opportunity to inject behaviors for feedback and tracing into the runtime behavior (and the IDE could also do this implicitly) (which have identity semantics and will be eliminated while not actively debugging).

For metaprogramming and 

First, a little discipline and best practice can go a long way: 

Developers can mitigate these concerns through discipline and best practices, e.g. to favor fine-grained modules with well-defined responsibilities, to avoid modules that make "shotgun" contributions to the compile-time environment. 

And developers are also granted some transparent capabilities to support runtime debugging or live programming (by 'transparent' I mean that they have the identity behavior as semantics).


Developers may also provide compile-time outputs (i.e. there are resources equivalent to a compile-time error log), or augment the generated behavior with 

To support live programming

using fine-grained modules with well-defined responsibilities. 


The compile-time environment is constrained so it can transparently be discarded after the AO is produced. However, Awelon is compatible with live programming, enabling updates to code in real-time.




Sealer Per Module
-----------------


Awelon Object Language (AO)
---------------------------

AO is a plain text language, closer to assembly than byte code.

        @ao v1.0
        

When Awelon is compiled, it targets an intermediate RDP-based Awelon Object language (AO). AO has rigid syntax and structure, a simplified type system, and carries swiftly verifiable proofs for the types it exposes. An AO is *pure* up to dependencies; one can understand AO as a pure function from a collection of behaviors to a collection of behaviors. 

AO supports code distribution and potentially some runtime specialization.

proof-carrying code

AO enables both code distribution and runtime optimizations at the RDP layer. 

The intermediate language is essential to support code distribution

Awelon is a compiled language. It targets an intermediate RDP-based Awelon Object language (AO)

which may then be compiled to machine code for the relevant host. 



Queries and answers, demands and responses, are *continuous* in RDP.

An important consideration is that a resource may be subject to multiple demands at the same time. 


Demand effects are much less expressive than imperative effects. In particular, a resource's state may only be influenced by the *set* of active demands, independent of ordering or duplication. 

RDP behaviors themselves are stateless. I.e. if you instantaneously stop a behavior and rebuild it from scratch, you will not lose any semantic information about the past. (You may lose cached or memoized values, and other regenerable information.)

Automatic Code Distribution
---------------------------




Design and Features Overview
============================

The following seconds document aspects of Awelon's design and structure.


Reactive Demand Effects
-----------------------

The nature of effects in RDP is simply that *a system's state may be influenced by its observers*. 

For example, if we query a drone for a good picture of a particular zone (specifying the zone in the query), the drone may volunteer to point its camera in that direction, or even divert its flight. This is possible due to the bidirectional dataflow of RDP: the query itself delivers information upstream about what is desired, and that information may be acted upon. Of course, it may be that our goal is to influence the behavior and we are not concerned about the response. In that case, the query is closer to a control signal and is said to express *demand*.

RDP is a dataflow model. Queries are continuous, long-lived, and may be modeled by subscriptions. Demands have the same properties - one can understand them as publishing values that influence targeted subsystems. If modeled in a publish-subscribe system, RDP would tightly couple every subscription with a published value. The published value may also be updated over time, modeling time-varying queries or demands. Thus, dataflow in both directions may be reactive, varying in time.

A system's state may be continuously influenced by the *set* of active demand values on that system. 

The word *set* is used in a strict sense: duplication or ordering within the set must have no semantic effect. This constraint is the foundation of RDP's equational reasoning properties (causal commutativity and spatial idempotence). These properties contribute to the declarative programming experience, and further enable many optimizations. Locally, the optimizations include stream fusion and pipline parallelism. Globally, optimizations include ad-hoc proxy caches or mirroring. If developers have a requirement to distinguish demands, for example to model a vote, they must do so by coupling each demand with a unique identifying token.

Commutativity and idempotence enable code to be rearranged, duplicated, and duplicates eliminated with a great deal of freedom. Refactoring impure RDP code is very similar to refactoring pure code, excepting that impure behaviors cannot be fully eliminated when the response is unnecessary. But pure code can be advantageous when reasoning about security. Awelon's type system can allow specification of pure behaviors.

External State
---------------

State is always just beyond the outer edge of an RDP system, in some abstract database. With external state, orthogonal persistence comes naturally, as does smooth handoff when updating code. Open extension is always feasible - i.e. we can always introduce additional behaviors to observe or influence the state. State may still be secure and modular, e.g. through cryptographic identity (HMAC, PKI, sealed values, sparse caps) or secure partitioning (chroot, sandboxing). But state isn't encapsulated.

State is assumed to be an *abundant* resource by most applications, meaning they have as much state as they want. It is only necessary that each state resource be specified by a distinct identifier (much like filenames in a filesystem). It is not difficult to develop schema that ensure different entities and relationships are associated with different states. 

However, which state models are available depends ultimately on the API provided to the application object. It would not be difficult to enforce a quota through the type system (e.g. by providng only a few finite-state resources).

State resources for RDP are different from those from imperative systems. Since demands are continuous, eventful update models do not make sense - e.g. what exactly does it mean to "add one" continuously? And there is no serialization or exclusive control: state must be influenced by the full *set* of concurrent demands. However, there are plenty of possible state models. For example, a pair with an initial state and an update behavior `(s0, Set w -> s -> s)` can specify a lot of simple discrete-varying, time-insensitive state models. Richer models might take information about time, or might be animated: updating even while demand stays constant.

It is feasible to allow developers to specify ad-hoc state models. This is effectively achieved by augmenting the identity of the state resource with information about its behavior. RDP cannot create 'new' state, but if state is *abundant* then there may be states of many kinds. A capability to access this ad-hoc state may be provided to the application object.

Awelon programmers can generally assume that state is persistent (unless it's naturally volatile) and I may pursue standard support for ad-hoc state.



Awelon's Module System
----------------------

Modules in Awelon are anonymous and cannot be imported by name.

The first word in each module specifies a language for parsing and preprocessing the rest of the module. Awelon allows specifying multiple small modules per file, each indicated with `@` at the start of a new line.

A compiler of Awelon code must have a few 'built in' languages to bootstrap the process, but further languages may be provided via the module system. 

In general, most languages will not allow developers to further manipulate the language (e.g. by defining macros). 


Awelon does not allow developers to manipulate the language from within the module


 The result of this process is a data structure representing the 

Some languages may be built in, while other languages may be provided through the module system.

 The syntax is based roughly on [ML9](http://www.sics.se/~joe/ml9/doc.html) from Joe Armstrong.


          
        @foo { k1=v1, k2=v2 ...} 
        code in foo 
        @bar
        code in bar

There are no constraints

    

 This isn't really a full language so much as a parser and preprocessor



Awelon supports user-defined syntax in a simple manner: each module opens with a simple language identifier, typically a short string identifying a language or specific version. This identifies the parser for the module. (If no parser can be found, a warning is raised and the module is ignored.)




Dynamic Behaviors
-----------------




Temporal Properties
-------------------

RDP has a number of explicit temporal properties that are implicit in most other paradigms.

### Logical Latency

Spreadsheets offer an illusion of *instantaneous* communication, which is feasible due to their locality and stateless nature. RDP cannot offer the same. It takes time to query remote resources. It takes time to perform computations. If we happen to express an open feedback loop (where a response influences demand influences response) then time is essential to understanding and modeling the loop. A feedback loop is a form of temporal recursion (always with an external resource). 

RDP models logical latency as a static property. In Awelon, this is modeled in the type system. Every value has a logical latency, a time at which it (logically) becomes available. The logical latency is often a conservative estimate, or based on real-time requirements, though it can potentially be based on empirical metrics.

Note that *logical latency* is distinct from an actual, implemented, *physical latency*. In practice, if logical latency is much larger than physical latency, this corresponds to buffering and may result in physical delays. And if physical latency runs higher than logical latency, there may be retroactive updates. 

Awelon programmers may inject latency into their code, or may rely on implicit latency from effects handlers. The most essential place to inject latency is for feedback loops. 

### Duration Coupling

An invariant of RDP is tight duration coupling: if you query a value over a continuous period of exactly 43.2 seconds, you will also receive a response signal over a continuous period of exactly 43.2 seconds. There may some latency between issuing the query and receiving the response, but it will be statically known and typically much smaller than the durations (e.g. 30ms). 

Relevantly, even if the query result is not immediately available, or there is an error, you will still receive a response for the full duration you maintain the query. But the response may need to include an error option, or the ability to indicate one is waiting on the full result. (A 'waiting' response is a trivial way to integrate asynchronous processes with RDP. A more interesting option is to provide partial results.)

Duration coupling enables RDP developers to reason about resource control and process control. In particular, programmers know that if they *stop* sending the signal, then the next behavior in the chain will also *stop* sending the signal, inductively all the way through the behavior. Resources tend to take advantage of this by returning to a passive or low-power idle state when there is no active demand. Developers rarely need to concern themselves with explicit cleanup.

### Logical Synchronization



### Speculation


RDP behaviors do not manage state internally - i.e. there are no 

RDP behaviors 

if there are multiple identical demand values, this has no additional effect on system state. Nor does the ordering within the set matter. 

two observers with the exact same demand value will not cause any additional effect, and neither is order of subscription relevant.

By *set



In general, the dataflow in both directions can be understood as a signal - a time-varying value. RDP developers do

Spatial Properties
------------------

### Location Types

### Heterogeneous Spaces

### Crossing

### Signal Distribution and Disruption






Distributed Programming
-----------------------



Awelon is a compiled language, but much compilation is expected to occur at runtime - to support distributed programming, live coding, and runtime specializations based on stable signals or configurations. Awelon will typically be distributed with source or some compact, preprocessed variation thereof.

To support distributed programming, live coding, and runtime specialization (based on stable signals), Awelon is often provided as a runtime compiled language. 

is a runtime compiled language, and is distributed largely in source form


Awelon is (generally) a runtime compiled language. It must be, to effectively support distributed programming, live programming, and late-stage optimizations for stable inputs. However, it may be used for traditional compiled systems. Awelon will be able to target JavaScript  in a processes.



RDP models
  real-time systems with static latencies, logical synchronization,
  and secure encapsulation of declarative effects. To this, Awelon
  adds such features as dependent types, sealers/unsealers, ad-hoc
  external state, substructural types, and effect handlers.

  Awelon is a compiled language, intended for runtime compilation.
  At runtime, we can specialize for stable inputs, or support open
  extension, live programming, and distribution, rebuilding programs 
  as needed. State in RDP is external to support continuous rebuild.
  Awelon is distributed primarily as source code. 

  Awelon is object capability secure: there is no implicit, ambient
  authority to access state or invoke effects. 

  Awelon is object capability secure: access to state resources or
  foreign 
   

  Awelon is not pervasively Turing Complete, and indeed is limited
  to expression of real-time functions up to use of external state
  resources. General purpose computation is modeled to occur over
  time. 

  Awelon is a general purpose language 
  Reactive Demand Programming is an effectful, declarative, reactive
  model for orchestration of open systems. Sirea implements RDP in
  Haskell. This particular module is the `core` of Sirea, just the 
  basic behaviors and implementation. Other packages will support
  specific domains or problems (UI, state, video, sound, etc.).

  RDP is similar to arrowized FRP, excepting how it manages effects
  and state. RDP behaviors can encapsulate access to resources and
  services, observing and influencing them through signals. State is
  modeled as an external service. (By comparison, FRP models state 
  as an internal resource with event accumulators or integrals, but
  is purely functional. FRP does not encapsulate access to shared 
  state or resources.) 
  
  RDP is effectful, but not imperative. The constraints on effects
  ensure RDP achieves many reasoning, refactoring, and abstraction 
  benefits of pure functional code. In particular, constraints for
  declarative expression are provided: commutative, idempotent, and
  associative expression; simple logical composition of effects as
  sets of concurrent demands. 
  
  Arrowized composition of behaviors protects RDP properties. Sirea
  developers, however, must be disciplined when adding new effects
  modules: many hooks between RDP and Haskell IO are not RDP safe.

  For more information, see the github Readme and linked pages.

