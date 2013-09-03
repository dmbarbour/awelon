## Capability-Based Security

If you are not familiar with the concept of object capability security, I recommend reading [Ode To Capabilities](http://erights.org/elib/capability/ode/ode-capabilities.html). Capabilities are an excellent way to manage authority, being very expressive and having most properties of [secure interaction design](http://zesty.ca/pubs/csd-02-1184.pdf). In context of RDP, capabilities are essentially behaviors passed as arguments to other behaviors. RDP is designed for distributed systems with runtime code-distribution between mutually distrustful systems; avoiding ambient authority is a very good idea. Capability security has been part of RDP's design from the beginning.

In Awelon, capability security can be enforced by eliminating [ambient authority](http://en.wikipedia.org/wiki/Ambient_authority) behaviors (e.g. effectful primitives in the AVM) and shifting authorities to application parameters. Capabilities in Awelon may be *linear*, which can further enforce certain security properties. Capabilities can be *static* (and usually are, in Awelon), which is very nice for optimization purposes. 

A good place for a programmer to store capabilities is the programmer's hands (part of Awelon's default tacit environment). By default, the contents of those hands are not passed to partially applied subprograms. This supports an intuition that it's the programmer holding the keys, and carefully granting authority to untrusted subprograms. 

But we can do better! 

#### Mixed Capability and Ambient Authority

With a carefully designed AVM, Awelon can easily support a *mix* of ambient authority and capability security. Each host can support two or more kinds of partitions: one with ambient authority, one without, and asymmetric communication between them. Distrusted developers can be forced to target the secured variation of the partition, but their code will receive capabilities that reach back into the ambient authority partition. 

There are at least a few advantages to this mixed design:

* More uniform application model from compiler's perspective.
* Developers have freedom to introduce new ad-hoc capabilities when necessary.
* Ambient authority, for all its insecurity, is very convenient.

Developers can create their own ad-hoc application models by essentially wrapping the main behavior.

        @appModel
        import frameworkStuff
        this = gatherFooCaps enterHighestSecurityMode runFooApp
        @app
        import appStuff
        this = meh blah boring %  assuming access to FooCaps

Instead of compiling `app` directly, then, developers simply compile `[app] appModel`.

This is the design that Awelon favors. 

#### Coupling Power and Responsibility

Linear types allow an API to say what a subprogram *must* do, and capabilities allow an API to say what a subprogram *can* do. They can be combined by requiring linear arguments to a capability, or by use of linear capabilities. This combination can be very expressive, while still supporting a great degree of flexibility in how things are achieved.

