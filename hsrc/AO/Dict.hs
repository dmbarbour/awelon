
-- | Tools to build and access an AO dictionary.
module AO.Dict
    ( AODef, AODictMap, AODict
    , buildAODict, cleanAODict, emptyAODict
    , readAODict, updateAODict
    , AODictIssue(..)
    , module AO.Code
    ) where

import Control.Monad ((>=>))
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as Set
import AO.InnerDict
import AO.Code

-- | An AO definition is a trivial pairing of a word with the AO code
-- that is inlined wherever the word is used. In practice, we want some
-- extra metadata with each definition (e.g. source location).
type AODef meta = (Word,(AO_Code,meta))

-- | A dictionary map is simply a map formed of AO definitions.
type AODictMap meta = M.Map Word (AO_Code,meta)

-- | Access the clean definitions within an AO dictionary.
readAODict :: AODict meta -> AODictMap meta
readAODict (AODict d) = d

-- | update metadata for a word (if it exists)
updateAODict :: (AO_Code -> meta -> meta) -> Word -> AODict meta -> AODict meta
updateAODict fn w (AODict d) = case M.lookup w d of
    Nothing -> (AODict d)
    Just (code,meta) -> 
        let meta' = fn code meta in
        AODict (M.insert w (code,meta') d)

-- | useful for initializations
emptyAODict :: AODict meta
emptyAODict = AODict (M.empty)

-- | to report problems with a dictionary while cleaning it up.
--
-- Override: multiple definitions are provided for a word, in the
-- order given. The last definition in this list is kept in the
-- dictionary, unless there's some other issue with it.
--
-- Cycle: a word is transitively part of a cycle, and thus its
-- definition cannot be fully inlined (as per AO semantics). Where
-- conventional languages use recursion, AO uses explicit fixpoint.
-- A good fixpoint combinator is: %r [%^'wol] %rwo^'wol 
--
-- Cycles are removed from the dictionary after being recognized,
-- such that any given word is recognized as part of at most one 
-- cycle. This may result in missing definition warnings.
-- 
-- Missing: a definition uses words that are not part of the
-- dictionary. All such definitions are removed from the dictionary.
-- 
data AODictIssue meta
    = AODefOverride Word [(AO_Code,meta)] -- multiple definitions for one word.
    | AODefCycle [AODef meta]             -- a cycle of words 
    | AODefMissing (AODef meta) [Word]    -- definition is missing words in list

-- a warning capability
type ReportIssue m meta = AODictIssue meta -> m ()

-- | given a list of definitions, produce a 'clean' dictionary (no cycles, 
-- no incompletely defined words) and also generate some warnings or errors.
buildAODict :: (Monad m) => ReportIssue m meta -> [AODef meta] -> m (AODict meta)
buildAODict warn = getFinalDefs warn >=> cleanAODict warn

-- build a map and report overrides 
getFinalDefs :: (Monad m) => ReportIssue m meta -> [AODef meta] -> m (AODictMap meta)
getFinalDefs warn defs =
    let mdict = L.foldl mmins M.empty defs in -- (word,(cm,[cm]))
    let dictOfFinalDefs = fmap fst mdict in -- (word,cm) 
    let lOverrides = M.toList $ fmap (L.reverse . uncurry (:)) 
                              $ M.filter (not . null . snd) mdict 
    in
    mapM_ (warn . uncurry AODefOverride) lOverrides >>
    return dictOfFinalDefs

mmins :: (Ord k) => M.Map k (a,[a]) -> (k,a) -> M.Map k (a,[a])
mmins d (k,a) = M.alter (mmcons a) k d where
    mmcons a0 Nothing = Just (a0,[])
    mmcons a0 (Just (a1,as)) = Just (a0,(a1:as))

-- | cleanup a map by removing words with cyclic or incomplete definitions
cleanAODict :: (Monad m) => ReportIssue m meta -> AODictMap meta -> m (AODict meta) 
cleanAODict warn = clearCycles warn >=> clearMissing warn >=> return . AODict

clearCycles :: (Monad m) => ReportIssue m meta -> AODictMap meta -> m (AODictMap meta) 
clearCycles warn d0 = emitErrors >> return cycleFree where
    g0 = fmap (aoWords . fst) d0 -- M.Map Word [Word]
    cycles = detectCycles g0 -- [[Word]]
    cycleFree = L.foldr M.delete d0 (L.concat cycles)
    emitErrors = mapM_ reportCycle cycles
    reportCycle = warn . AODefCycle . fmap getData 
    getData w = (w, fromJust (M.lookup w d0)) -- recover def & meta
        -- by nature, any word in a cycle must be defined
        -- so 'fromJust' is safe here

-- detect cycles in an abstract directed graph 
-- using a depth-first search
detectCycles :: (Ord v) => M.Map v [v] -> [[v]]
detectCycles = deforest where
    deforest g = case M.minViewWithKey g of
        Nothing -> []
        Just ((v0,vs),_) -> 
            let (visited, cycles) = dfs g [v0] [v0] vs in
            let g' = L.foldr M.delete g visited in
            cycles ++ deforest g'
    dfs _ cx _ [] = (cx, [])
    dfs g cx p (v:vs) = 
        case (L.elemIndex v p) of
            Nothing -> -- no loop; may need to visit 'v'
                if L.elem v cx then dfs g cx p vs else 
                let (cxd,cycd) = dfs g (v:cx) (v:p) (edgesFrom g v) in
                let (cxw,cycw) = dfs g cxd p vs in
                (cxw, cycd ++ cycw)
            Just n -> -- loop found; 'v' necessarily has been visited 
                let cyc0 = L.reverse (L.take (n + 1) p) in
                let (cxw,cycw) = dfs g cx p vs in
                (cxw,(cyc0:cycw))
    edgesFrom g v = maybe [] id $ M.lookup v g

-- keep only words whose definitions are transitively fully defined
clearMissing :: (Monad m) => ReportIssue m meta -> AODictMap meta -> m (AODictMap meta) 
clearMissing warn d0 = emitErrors >> return fullyDefined where
    g0 = fmap (aoWords . fst) d0 -- 
    mwMap = incompleteWords g0
    fullyDefined = L.foldr M.delete d0 (M.keys mwMap)
    emitErrors = mapM_ reportError (M.toList mwMap)
    reportError = warn . mkWarning
    mkWarning (w,mws) = -- w is in dictionary; mws aren't
        let defW = fromJust (M.lookup w d0) in
        AODefMissing (w,defW) mws

-- find incomplete words based on missing transitive dependencies
-- the result is a map of word -> missing words.
incompleteWords :: (Ord w) => M.Map w [w] -> M.Map w [w]
incompleteWords dict = dictMW where
    (dictMW, _okSet) = L.foldl cw (M.empty, Set.empty) (M.keys dict)
    cw r w = 
        if (M.member w (fst r) || Set.member w (snd r)) then r else
        case M.lookup w dict of
            Nothing -> r -- w is missing word; will capture in 'mdeps' below
            Just deps ->
                let (dmw, dok) = L.foldl cw r deps in
                let mdeps = L.filter (`Set.notMember` dok) deps in
                if null mdeps 
                    then (dmw, Set.insert w dok) -- add word to OK set
                    else (M.insert w mdeps dmw, dok) -- add word to missing words map

