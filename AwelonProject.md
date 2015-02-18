
# What is Awelon Project?

A new HCI model, with a code as material metaphor, to unify PX and UX.

## To unify PX and UX

My goal is to unify programmer experience (PX) with the user experience (UX).

This is an elusive goal. It has been pursued for many years, with many different hypotheses for what such a unification might entail and how it might be achieved. Related projects include Squeak Smalltalk, ToonTalk, LambdaMOO, Morphic, Croquet, Emacs, and HyperCard. A relatively successful effort to unify PX and UX was the Unix command line, where users would build short programs of process pipelines. But that has been marginalized by the development of GUI.

To me, the unification of PX and UX means that programming becomes a casual effort, such that users make it part of their normal workflow and don't even think about it as programming. Example user stories:

* A casual investor watches several stocks. For a couple days, he goes from one website to another and looks at each stock's value. Then he tires of this and decides to toss all the values into a spreadsheet so he can watch them on a single page. Later, the investor decides to add an e-mail alert when a stock goes above a certain value, and does so by attaching a rule (copied from a tutorial website) to the stock value in the spreadsheet.

* A visual artist has an idea of creating automatic image quilts based on Google Image searches. He does so by grabbing the search bar and corresponding results off the Google website (after selecting image search), then providing a few examples of what an image quilt should look like given a set of images ([programming by example](http://en.wikipedia.org/wiki/Programming_by_example)), and verifying the generated function against a few searches by hand. He then e-mails the resulting widget to his friends for testing, and eventually shares it through his blog (as a live example). 

* A young musician who watches a lot of Youtube in his spare hours has recently learned about [microsounds](http://en.wikipedia.org/wiki/Microsound). With a little digging, the musician encounters a cloud-based service that will automatically extract and cluster microsounds. The musician has an idea: to transform music by extracting microsounds from music and replacing it with similar microsounds (according to the clustering) from an arbitrary subset of Youtube videos. So he quickly glues the services together together with a short function for replacing one microsound by another of the correct subset. The pipeline is optimized in the background, such that nothing is downloaded to the musician's machine except the final result, and the video isn't transferred at all.

The unification of PX and UX means that people who aren't professional programmers - artists, musicians, scientists, investors, soccer coaches, etc. - are able to think in terms of simple, logical capabilities:

* I can get an X.
* I can get a Y with an X.
* Therefore, I can get a Y.

Sadly, today's user interfaces create painful barriers:

* \[Application 1\] I can get an X.
* \[Application 2\] I can get a Y with an X.
* ???
* profit

Unifying PX and UX is not the same as making user interfaces programmable. It is not the same as *coding* or teaching people to *read or write code*. Rather, **unifying PX and UX is about eliminating the barriers between applications**, making it easy to develop new tools and and share them with a community.

There would still be a role for professional programmers, much as there is a role for professional writers or professional cooks. But I think programming can be marginalized as a career, i.e. such that professionals are only needed in 5-10% of the cases where we need them today.

## A code-as-material metaphor

Today, most programmers think about programming as similar to a speech act. They understand code by reading it, perhaps simulating its behavior in their head. Code tells the computer what to do, oft in an imperative mood.

An alternative is to understand programs as something more like a physical machine, constructed of materials that behave in predictable ways. You understand a machine by observing and experiencing its behavior over time, and by having some global concepts such as locality of physical interactions. You build larger machines by putting together pieces you already comprehend.

A hypothesis of Awelon project is that a code-as-material metaphor will better serve unification of PX and UX. Users should gain a fair comprehension of software components through experience and exposure. A little education or experiment would help fill the gaps.

Of course, like physical material, not all code is easy to use or understand. Like glass, code can be fragile, difficult to modify or extend. Like roots of a tree, code can be deeply entangled with its environment, difficult to extract, or reuse. Like water and cesium, not all code is safe for use in the same system. As programming materials go, we want the analog of [Lego](http://en.wikipedia.org/wiki/Lego) or [Meccano](http://en.wikipedia.org/wiki/Meccano), except cheaper to copy and share.

Much of Awelon project's design has been oriented around developing code that is safe and easy to modify, extend, disentangle, extract, reuse, and reason about with global principles. The result is [Awelon Bytecode](AboutABC.md) and [Awelon Object](AboutAO.md) languages.

See also: Conal Elliott's [Tangible Functional Programming](http://conal.net/papers/Eros/).

## Quality through Quantity

While there is much open source code available today, it is typically difficult to access or reuse in a different context than it was originally developed. Hurdles to reuse include packaging models, entangled dependencies, import boiler plate, namespaces, hidden side-effects. But if we could dodge those problems, then [quantity becomes a major asset](https://lukepalmer.wordpress.com/2010/07/22/programming-for-a-culture-approaching-singularity/).

Awelon project languages are designed to mitigate and avoid these issues. Communities and individuals maintain dictionaries of words, and those words are easily shared and adopted. Users have the full power of their community at their fingertips and will be capable of expressing interesting applications in the space of a tweet or URL, limited more by knowledge and imagination than by artificial hurdles. 

## A new HCI model

Effective use of the code-as-material metaphor for unification of PX and UX requires exposing users directly to software components. Applications and services should be constructed of widgets and wires and rules, exposed (albeit in an aesthetically subtle manner) such that users gain understanding both of the applications and services they use, and of potentially reusable subcomponents. Further, those applications and services should themselves be reusable software components. If an application is composed of widgets wired to external data, then the application as a whole should be a widget wired to external data. 

Users should be navigating and manipulating a system of graphs, diagrams, documents, geometries, functions, sound and video, and other software components with the same ease they navigate files, folders, and web-pages today. 

Historically, such 'IDE is the UI' concepts have been rejected. 

I believe that much blame for this failure should be pinned on the programming languages involved. For example, objects in Smalltalk become too easily and invisibly entangled with other objects in the environment. This makes it more difficult to copy and share objects compared to the code from which they derived. Further, as the state of an object diverges from the source code, it becomes too difficult to update the code and achieve a sane update to the behavior of existing objects. 

Awelon bytecode is a much more promising medium.

Further, Awelon project's HCI design includes two important new principles:

First, we **model user input as streaming code**. This unifies user actions as acts of programming at the lowest level. A user's input history can then be mined to develop user-macros, tools, and functions based on example. The tools so developed are easily shared with the community as first-class objects, so this feature supports everyone even if only a few users directly leverage it.

Second, we **represent long-running behaviors and policies as accessible objects**. This supports user awareness and control over what their system is doing, e.g. an ability to halt a process, update code, or revoke authority.

Awelon project supports the first principle through a streamable Awelon bytecode, and the second principle through a combination of [linear types](http://en.wikipedia.org/wiki/Substructural_type_system) (so we can't lose access to a live behavior) and [Reactive Demand Programming](AboutRDP.md) (so we can easily halt or update the behavior).

There is also an interesting middle ground between short-lived user actions and long-lived behaviors. We can have objects that only become 'active' with presence or observation of the user, e.g. modeling something like a field effect. Many applications would fit this middle ground.

