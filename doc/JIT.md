
I've been working for a little while on a Just-In-Time compiler for ABC. 

In context of the Haskell based AO runtime, a JIT compiler can be implemented by translating ABC code into to Haskell code then using System.Plugins to compile and load the Haskell code at runtime. 

A naive implementation of this has already been achieved. But the naively translated code performs quite poorly - i.e. it takes a long time to compile, and when done it runs slowly compared to the interpreted code. A recent goal has been to develop a better ABC to Haskell compiler.

I've been thinking a bit about my approach here...

Currently, I'm writing my ABC to Haskell compiler in Haskell. However, if I were to shift this compilation task into AO, I could presumably develop a fair bit of the AO libraries at the same time, and I could perhaps speed up the development effort in some useful ways.

Still, should I finish up a better ABC-to-Haskell compilation? The earlier I have a good JIT for ABC, the earlier I can begin to use it for real applications.


