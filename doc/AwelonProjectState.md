

Shared state is modeled as an external resource in RDP and ABC. 

Goals includue:

* Ability to define the state model.
* Ability to upgrade the state model.
* Ability to model one-to-one, one-to-many, many-to-one, many-to-many state

Requirements:

* State models should not be directly accessed and manipulated in the first stage. The reason for this: the specification of the state model would be easily lost to history, and difficult to access or update.

* All persistent state must be controlled by an agent, to enable update of the state model.

Hypothesis: to upgrade state model, it must be controlled by an agent. All state resources should start as exclusive state resources, using a stable uniqueness source. (Need some way to support stable discovery, so perhaps have Chord/DHT underlying everything.) Open state (DHTs, etc.) can be modeled as participating/contributing to a collaborative state model.

Hypothesis: source stable uniqueness is important even for the history of the user input stream. Relevantly, we must be able to edit the past. However, perhaps source stable uniqueness could be the default but effectively optional as an abstraction inversion to modeling automatic/incremental identities for some purposes. An advantage is that this provides URL-like uniqueness.

Uniqueness, however, might be available. 




