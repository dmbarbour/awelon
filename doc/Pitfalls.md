
# Pitfalls to Avoid

Reminders to self regarding design pitfalls.

## Conditional Capabilities (Do not!)

When brainstorming, one idea was a convention of the form `{?foo}`, whose output was a sum type depending on whether the environment recognizes `{foo}` as a valid capability. This initially seems neat because it results in code adaptable to the environment. However, the properties are simply *wrong* for adaptive code. 

The question of whether a capability is available should be resolved at the point the capability is acquired, upstream of where it is applied. This enables a bigger picture view of what resources are available for developing adaptive code. This separation of concerns also improves portability, extensibility, configuration management, testability with mockup environments, and security.

## Conventions for Defining Symbols in ABC (Do not!)

Several times, perhaps due to FORTH, I've been tempted to support a user-defined symbol extension from within ABC. Each time, I determine this is a very bad idea. However, it seems easy to forget why. An example such mechanism was:

        : :: N(c) * ([x→y] * e) → e  -- DO NOT
          where `c` is a UTF-8 character
          and the block contains meaning of that codepoint

This feature is a terrible idea because the scoping is unclear, and there is no solution for it. E.g. if each block defines each symbol, we lose compression and decomposition; otherwise, we lose metacircularity, security, and equational reasoning. There are also concerns about expressing cycles, and having two distinct ways to abstract code.

The same problems occur for capabilities to define capability texts. Don't.

However, ABC can address the issue of compression external to the ABC semantic layer. E.g. we could use streaming compression below UTF-8, or use `{#secureHash}` references to name large templates and frameworks, or use ABCD which adds a global dictionary to ABC.

