{-# LANGUAGE ViewPatterns #-}

-- | a simple module to help display the AO standard environment
module ShowEnv (showEnv) where

import qualified Data.List as L
import ABC.Imperative.Value

-- | print an AO standard environment, or a summary of it, for
-- the user to view.
showEnv :: V cx -> ShowS
showEnv (P s (P h (P pb (P (P (valToText -> Just sn) ns) ex)))) =
    showPower pb . showExt ex . showNamedStacks ns . showHand h .
    showString "---" . showString sn . showString "---\n" .
    showStack 12 s
showEnv v = 
    showString "--(non-standard environment)--\n" .
    summarize v . showChar '\n'

-- print a value, but not all of it if it's enormous
summarize :: V cx -> ShowS
summarize v =
    let sumSize = 1000 in
    let sVal = show v in
    let (sSummary, sChopped) = L.splitAt sumSize sVal in
    showString sSummary . 
    if null sChopped then id else showString "\n...chopped"

showPower, showExt, showNamedStacks, showHand :: V cx -> ShowS

stackCount :: V cx -> Int
stackCount U = 0
stackCount (P _e s) = 1 + stackCount s
stackCount _v = 1

showStack :: Int -> V cx -> ShowS
showStack _ U = id
showStack n s | (n < 1) = 
    showChar '(' .
    shows (stackCount s) .
    showString " more)\n"
showStack n (P v s) =
    showStack (n-1) s .
    showChar '|' . summarize v . showChar '\n'
showStack _ v = 
    showString "(non-standard stack): " .
    summarize v 

showNamedStacks U = id
showNamedStacks (P (P (valToText -> Just name) stack) ns') =
    showString name . 
    showString ": " . shows (stackCount stack) . 
    showChar '\n' . showNamedStacks ns'
showNamedStacks v = 
    showString "???: " . shows v . showChar '\n'

showHand h =
    let n = stackCount h in
    if (0 == n) then id else
    showString "hand: " . shows n . showChar '\n'

showPower (B _) = id
showPower v = showString "power block: " . summarize v . showChar '\n'

showExt U = id
showExt v = showString "extended env: " . summarize v . showChar '\n'

-- TODO: consider switching to a 'summary' value display.


