
### Incremental Processes

Every loop expressed in ABC should terminate. A compiler is allowed to reject a program if it proves non-termination, and may complain if it cannot prove termination. The traditional `while(true)` process loops of imperative programming are not allowed. This is a good thing! Those `while(true)` loops have *awful* properties for composition, extension, and reuse.

ABC systems instead model long-running behaviors more explicitly:

* RDP behaviors
* streaming ABC
* incremental processes

ABC is designed primarily for RDP, and Awelon project makes heavy use of unbounded streaming ABC for serialization and environment manipulation. But if ABC used for imperative processes, an incremental process model should be favored. In that case, each process might have a structure similar to:

        type Process a b = a -> (b, Process a b)

At each step, a process will perform a finite (usually small) incremental amount of work. These processes can be composed sequentially or in parallel, or model ad-hoc workflows. These processes can model environments, encapsulating more processes and modeling connectivity or dispatch. The process graph can be dynamic, changing from step to step, represented by returning the process for the next step. Large, specialized processes are constructed from smaller, reusable ones. Process control is implicit with simple behavior: we can pause between steps. 

In ABC, this model of incremental processes can generally be parallelized, fused, and optimized to a very high degree (based on causal commutativity, fast and loose reasoning). 



