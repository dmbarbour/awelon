

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

..................

Another interesting possibility just struck me: when modeling the 'stream' of actions influencing the environment, perhaps I should model it as follows:

        type Process a b = a -> (b, Process a b)
        type AwelonProjectStep = Process (input * env) (renderable * env')

Effectively, the operation on the environment is an evolving process that is not an observable part of the environment, yet also can be statefully evolved. The main issue with this design is that it makes the process itself inaccessible, a violation of the 'no hidden long-running behavior' principle.

But if I use an ABC environment where the Process at each step is definitely part of the environment, and subject to update or change... that might work well, but it also might be too frozen. OTOH, perhaps that would be acceptable, so long as the process has a 'private space' for its own manipulations.

I need to think about this further, exactly how I want to build the stream of actions based on user inputs. It seems useful to encode this within Awelon project itself, but in a safer way. 

Perhaps some kind of automatic rewriting? Certain structures in the user's environment can interact with the process, or certain built-in gestures or button actions, basically adding and removing objects from the environment on a larger scale.






