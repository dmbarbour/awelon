
To use this 'aodict' package:

In this directory, call `ao dict2hs`, which should create an `AODict.hs` file. (You'll need to repeat this step whenever you have updates to the dictionary.) Then simply invoke `cabal install`. Eventually, construction of the dictionary may be automated.

If you cannot call `ao dict2hs`, you may need to install the `ao` package and ensure that `~/.cabal/bin` is in your environment's `PATH`. 

In addition to the `AODict` module, this package provides two executables: 

  `aoTest` runs the same tests as `ao test`, but much more quickly (ignoring compile time!)
  `aoExec` will run a single word, allowing extra arguments as a list of strings on the stack.

Using flags at cabal, developers can switch from using Rational to using Double (which offers ~ 50% performance improvement for simple adder loops). Not all tests will pass if using doubles. 

