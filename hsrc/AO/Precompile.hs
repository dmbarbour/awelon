
-- | This is an idea for partially pre-compiling the AO dictionary
-- by recognizing words starting with `#` for separate compilation
-- and linking. This enables a very flexible mix of compiled code
-- together with interpreted code.
--
-- Words such as #foo are substituted by {#secureHashOfBytecode}. A 
-- map is generated containing these secure hashes and bytecodes.
-- Further compilation is left to the runtime.
module AO.Precompile ( preCompileAO, PreCompD ) where

import qualified Data.Map as M
import qualified Data.List as L

import AO.Code
import AO.Compile
import AO.InnerDict

import ABC.Operators
import ABC.Hash

type PreCompD = M.Map Text [Op]

preCompileAO :: AODict md -> (AODict md, PreCompD)
preCompileAO dict =
    let 



