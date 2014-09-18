This repository contains multiple components developed for Awelon Project.

Components:

* [ABC](AboutABC.md)[1](ABC.md) - the Awelon bytecode
* [AO](AboutAO.md) - a macro language for the bytecode
* [RDP](AboutRDP.md) - a reactive paradigm for open systems

Vision: 

* [Awelon Project](AwelonProject.md)

# Status

Preliminary Software:

* **aoi** - a REPL for AO code, written in Haskell
* **ao** - a multi-utility command line, written in Haskell
* A small, growing dictionary of AO code (the `ao/` directory)

Awelon project is not in a good condition for showing off. Its interpreted performance is lacking, and I'd like to avoid ABC gaining a reputation as 'slow' before it implements a proper set of compilers and optimizers. Further, the AO dictionary is still in a very early stage, e.g. implementing useful data structures and other low level stuff. It will be a while before we can express interesting applications. 

However, people who aren't going to be judgemental about performance are welcome to experiment with and contribute to the AO dictionaries.

In the near future, I'll be developing a wiki-based web application for further developing AO code. This will ultimately make contribution a lot easier, and enable development of simple web application services.

## Environment Configuration

For `aoi` and `ao` developers must adjust their environment variables.

**Environment Variables:**

* **AO_PATH**: where to search for AO files; no default
* **AOI_DICT**: imports for `aoi` executable; default "aoi"
* **AO_DICT**: imports for `ao` executable; default "ao"
* **AO_TEMP**: directory for JIT and other tasks; default "./aotmp"

Developers must set AO_PATH. Usually, this should just be a single directory with all the AO files (AO has a flat namespace anyway, and should eventually upgrade to a database).
