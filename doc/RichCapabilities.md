
In ABC, we can represent a token with form `{token}`. When invoked, the text within is passed to the environment along with the tacit argument. This allows invocation of side-effects, performance annotations, linking external components, and other features. Some conventions are applied to the interpretation of the text.

Within ABC, these tokens cannot be forged. There is no way to generate a token from a string, nor any way to observe the text within a token. This allows tokens to be used with object capability security model. In open systems, the token text should be protected cryptographically - e.g. by HMAC or encryption - to guard against external attack.

In a distributed system, a token representing access to some resource must typically be delivered back to its home machine before it can be used in a computation. 

However, for performance purposes, it may be useful if some extra conventions are followed such that a token can be manipulated without actually sending it home. 

This may be feasible with [Homomorphic Encryption](http://en.wikipedia.org/wiki/Homomorphic_encryption), or perhaps by simpler techniques - e.g. we could presumably *remove permissions* (or allowed messages) from a capability, supposing each permission is granted by an individual HMAC. I think there are a lot of special-case variations we could support.

But a relatively simple and general approach is to use public key encryption to capture ad-hoc sequences of input messages following a certain convention - e.g. capture inputs on the left and return an updated capability; apply those on the right. So let's use it as the initial basis for rich capabilities in distributed systems. 

## Rich Distributed Capabilities 

A rich capability consists of three parts:

* a single, previously unused character prefix - perhaps ◈ (U+25c8)
* a public key, which also serves as a unique resource identifier
* an encypted 'captured message' section (NEVER empty)

        {◈publickeypublickeypublickey contentcontentcontentcontentcontent}

These capabilities will may grow BIG, e.g. measuring up to a few megabytes. To optimize performance in ABC's UTF-8 representation, we'll use a base64 encoding.

New messages are added by essentially taking the current content, adding a separator, adding a random buffer, then adding a sequence of messages. 

        contentcontentcontentcontentcontent:randomrandom|msg1|msg2|msg3|msg4...

Then we encrypt this whole thing together again with the public key, thus generating an updated content string. The updated capability is now:

        {◈publickeypublickeypublickey content2content2content2content2content2}

Of course, it's going to be considerably longer than the older version. 

Fortunately, by supporting multiple messages in batches, we can feasibly limit the number of encryption and decryption steps based on how many (mutually distrustful) machines handle the capability rather than the absolute number of messages. For additional performance, we might also send this capability back home to its 'owner', who can perform the decryption and simplify the resulting capability.

The *initial* content must contain an additional secret, such that nobody can create new initial content using just the public key. Instead, clients are limited to monotonically extending this capability. 

The rich capability will receive `(a+b)` messages. The `a` messages are used to extend the content, and return an updated capability. The `b` messages are used for communicating with the resource.

