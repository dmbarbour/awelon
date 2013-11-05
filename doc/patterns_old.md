### Capability Distribution Patterns & Powerblocks

In ABC code, blocks are opaque. There is no primitive bytecode that can convert a block to text. A system may provide reflection capabilities to peek under-the-hood. There can be value in withholding this capability from an untrusted subprogram. 

Capability invocations are generally encapsulated within blocks. This offers two advantages. First, the capability text is protected from casual observation. Second, the capability text needn't be generated except for the rare capabilities that are serialized or observed.

Within Awelon project, capabilities are generally distributed via a centralized **powerblock**. 

The powerblock is a linear block that can be queried for capabilities. Each query returns the result and an updated powerblock. (A linear powerblock may provide linear capabilities, which would be removed rather than copied.) The powerblock is generally kept at a stable location in an Awelon environment, such that definitions can easily locate it. Under this convention, capabilities can essentially be treated as ambient authorities. Programmers can control authorities granted to a subprogram by forking the powerblock into parent/child, then constraining the child. 

To forking a powerblock, we must provide a unique identifier. 


Essentially, a powerblock is a representation of the *external* environment accessible to a program. 

A powerblock - relative to a collection of naked capabilities - makes it easier to model and enforce security policies, preferences, overrides, auditing. More precisely, a powerblock can be composed with other blocks, or may have logic injected, in a manner way that cannot be bypassed by the client of the powerblock. This logic also supports more flexible queries, i.e. multiple names for a capability, which could help with internationalization. 

### Pure Functional Programming

ABC is effectively a pure functional bytecode, modulo invocation of capabilities. Interestingly, use of capabilities also enables ABC users to enforce that particular subprograms are 'pure'. We can 

ABC operations effectively take an immutable environment value as input and produce another as output. Relevantly, there is no aliasing within the environment, and there is no access to, and output a new environment value.

ABC is not a pure language, due to capabilities. However, ABC's environment is lin


## Sealed Values and Encapsulation

A useful application of capabilities is to model sealer/unsealer pairs. The idea is that we have a pair of capabilities, one of which can seal values such that only the other can unseal them. Sealed values are opaque. There is a good opportunity here for type-system integration: the identity of the sealer can become part of a sealed value's type.

Sealer/unsealer pairs are useful for modeling identity, encapsulation, first class abstract data types, rights amplification, and a variety of other features. Awelon project makes heavy use of this pattern, even statically (i.e. some values can be sealed statically to model module systems, private data).

New, unique sealers can be constructed given a uniqueness source.

## Shared State Resources

Capabilities can access external state resources. In some cases, those resources are also accessible by other agents and services. A powerblock will generally provide access to some collaborative spaces (perhaps with varying levels of security) to publish or discover values. 

By providing a globally unique value, one can exclusively bind external state resource. This enables users to define or update their own state models, enforce many-to-one or one-to-many patterns, and better comprehend who has access to state. In Awelon project, this pattern is used extensively; while there are some external spaces not controlled by any particular agent, those are primarily for volatile communications, publishing services, discovering resources, and bootstrapping connections.

## Stable Uniqueness Source

Sealer/unsealer pairs, exclusive state, identity values, and other constructs require uniqueness. Further, these unique values should be stable across changes in source code, to be robust in presence of persistence or update. Awelon models a uniqueness source as a no-copy block that encapsulates a unique capability. 

Functions are pure, and RDP behaviors are idempotent; in both cases the same output must be returned for the same input. Thus, to have a formally unique output requires a formally unique input. Uniqueness sources cannot be 'created', only used if they already exist. Uniqueness sources in Awelon project are provided as initial arguments, otherwise they would not exist at all. 

A uniqueness source cannot be copied (or it would no longer be unique). However, it may be partitioned to arbitrary depth. For stability, it is necessary that the partitions be stable to rearranging, adding, or removing most code. In Awelon project, this is achieved using a parent/child metaphor, similar to directories in a filesystem. A parent must give each child a unique name, but the child has access to a fresh set of names.

In Awelon project, uniqueness is distributed in two distinct stages. In the first stage, user actions are modeled as pure functional streaming ABC code that manipulate the environment. In the second stage, the environment's structure is interpreted as an RDP behavior, which may continuously influence and observe the real world. Unique values in first stage enable objects to be moved without losing their identity or unique external bindings. Unique values in second stage enables dynamic state. 

Both stages use the same stability model. This consistency is valuable in case of history rewriting or programming by example.

In the first stage, however, developers can only distribute uniqueness sources. Capabilities to utilize uniqueness don't become available until the second stage. (Thought: It may be necessary to model the transfer from first stage to second stage uniqueness.)

