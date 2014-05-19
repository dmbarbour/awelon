
In ABC, we can represent a token with form `{token}`. When invoked, the text within is passed to the environment along with the tacit argument. This allows invocation of side-effects, performance annotations, linking external components, and other features. Some conventions are applied to the interpretation of the text.

Within ABC, these tokens cannot be forged. There is no way to generate a token from a string, nor any way to observe the text within a token. This allows tokens to be used with object capability security model. This can be useful, even for resources that don't need special protection, because capability security makes it easier for developers to reason about their own code. ABC potentially allows ad-hoc tokens to be embedded, but AO only permits hard-coding of a small subset: `{&annotations}` and `{:sealers}` and `{.unsealers}`. Some ABC streams may be similarly limited.

While tokens cannot be forged within ABC, they may be forged at the virtual machine layer. We can guard against this using cryptographic mechanisms, e.g. HMAC or encryption or large random numbers. The idea is to render the forgery of a token cryptographically intractable.

## Manipulating Cryptographic Capabilities

It may be useful to *refine* or *attenuate* capabilities in various ways, e.g. removing privileges or overriding certain functions. If computation is isolated to a single virtual machine, this is not difficult - simply isolate capabilities within a block, or generate a new one in response to a message. But if capabilities are distributed, it can be expensive to phone home every time we refine or manipulate the capability.

An interesting possibility is to interact directly with a cryptographic capability. I can think of a few ways to do this:

* send a sequence of 'messages' to the capability, e.g. such that each message represents an operation to refine the capability. 
* use homomorphic encryption to directly manipulate representations within the crypto-text.

The first option is relatively easy:

        Start with a capability
        {◈publickeypublickeypublickey contentcontentcontentcontentcontent}

        Extend the content with some messages 
        contentcontentcontentcontentcontent:randomrandom|msg1|msg2|msg3|msg4...

        Encrypt this whole sequence using public key
        content2content2content2content2content2

        Generate the updated capability
        {◈publickeypublickeypublickey content2content2content2content2content2}

This capability might have input type `(a+b)`, using `a` as messages to refine the capability, and `b` to finally execute it. The initial content must contain an initial secret, such that the whole content cannot be replaced. The resulting capabilities may grow very large and bulky (possibly many megabytes). However, this approach is relatively simple, and capable of many ad-hoc refinements, and the amount of encryption necessary is more a reflection of how often the capability changes hands between mutually distrustful subsystems, than of the number of refinements.

Homomorphic encryption is a more interesting approach, which I have not studied extensively. But I think we could cover a lot of security use-cases with some common homomorphic structures, and this is very much worth studying.

## Secure Hash Sources

I plan to broadly support a convention for capabilities to link external code and data. I call these *secure hash sources*:

        {#securehashsecurehashsecurehash...}

The secure hash (I favor SHA3-384) is used to identify, locate, download, and validate the code. The code is then semantically inlined in place of the token, as if it were there all along. This is a very simple semantics, and can support: software components, templates, frameworks. Via caching, we can also support separate compilation, potentially with much heavier optimizations than we would otherwise apply to streaming code.

(Of course, you cannot *update* code that will be validated by secure hash. Update must be addressed more explicitly in other layers.)

I believe that secure hash sources adequately address many common requirements for which we might use common resource identifiers, allowing developers to focus on more interesting problems.

## Communication Models

As another useful, widespread conventional capability, I believe I'll provide a generic way to communicate between Awelon hosts, e.g. by using a secure hash of a public key as the identifier for a host, and using various session identifiers.

## Distributed Resources: a Bad Idea

There are some tokens that don't truly need to be protected - i.e. because the security risk is minimal, or the resource is widely accessible anyway. In these cases, the resource really is 'ambient' in some sense, though (to simplify reasoning about or program code and subprogram behavior) we might still guard access to them within a program.

We might find it convenient to access these ambient resources using the same token across many machines. ABC can allow for this, though it requires some common conventions for interpreting token text (in a sense, the different virtual machines must have a compatible 'type'). A few potential examples:

* access to logical or physical time information 
* access to Tahoe-LAFS cloud services (encrypted files)
* access to HTTP `GET` or `PUT` methods for global domains 
* access to SMS or phone services

However, while this is feasible, it seems to be *a bad idea* to use in the general case.

First, there are subtle security issues. The HTTP option, for example, is dangerous in context of local domains or reverse domain lookups. We don't want someone logging into your router via HTTP GET and PUT. Similarly, access to physical time might expose a program to covert channels or timing attacks. Hammering out various security issues is good, but avoiding them for most Awelon VMs is better.

Second, a large set of these features would greatly complicate the Awelon runtimes, and require more maintenance and updates of them. Further, code using such 'distributed capabilities' would be sensitive to version.

We should almost always consider the more explicit, more securable alternative: a remote host is also free to *grant* some common capabilities to anyone who asks, and we can certainly keep these stable for long periods of time. We might simply create a framework to ask for these capabilities when we need them, and perhaps cache them. This alternative might give us most of the benefits of ambient authority by simple means of explicitly modeling it, while allowing heterogeneous authority when we wish for it.

While this technique might not be suitable in general, it might be reasonable for small subsets of Awelon virtual machines - e.g. those in a common 'cloud' configuration - to accept common tokens as if they were one larger virtual machine.

