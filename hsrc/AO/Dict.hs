
-- | Tools to build and access an AO dictionary.
module AO.Dict
    ( AODef, DictMap
    , AODict, buildAODict, readAODict
    , AODictIssue(..)
    , module AO.Code
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import AO.InnerDict
import AO.Code

-- | An AO definition is a trivial pairing of a word with the AO code
-- that is inlined wherever the word is used. In practice, we want some
-- extra metadata with each definition (e.g. source location).
type AODef meta = (Word,(AO_Code,meta))

-- | A dictionary map is simply a map formed of AO definitions.
type DictMap meta = M.Map Word (AO_Code,meta)

-- | Access the clean definitions within an AO dictionary.
readAODict :: AODict meta -> DictMap meta
readAODict (AODict d) = d

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
    | AODefCycle [AODef meta] -- a cycle of words (all removed)
    | AODefMissing (AODef meta) [Word] -- definition is missing words in list

-- a warning capability
type ReportIssue m meta = AODictIssue meta -> m ()

-- | given a list of definitions, produce a 'clean' dictionary (no cycles, 
-- no incompletely defined words) and also generate some warnings or errors.
buildAODict :: (Monad m) => ReportIssue m meta -> [AODef meta] -> m (AODict meta)
buildAODict warn defs = 
    getFinalDefs warn defs >>= 
    cleanAODict warn >>= 
    return . AODict

-- build a map while tracking definition overrides
-- then build 
getFinalDefs :: (Monad m) => ReportIssue m meta -> [AODef meta] -> m (DictMap meta)
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

-- cleanup the AO dictionary
cleanAODict :: (Monad m) => ReportIssue m meta -> DictMap meta -> m (DictMap meta) 
cleanAODict _warn = return -- TODO!  
    
    
{-

-- raw dictionary from files; warns for parse errors and overrides
buildRawDictionary :: [(Import, DictFile)] -> (S.Seq ErrorText, Dictionary)
buildRawDictionary dfs = (errors, rawDict) where
    errors = parseErrors S.>< overrideWarnings 
    dfsErrorTexts = map (S.fromList . map snd . df_errors . snd) dfs
    parseErrors = foldr (S.><) S.empty dfsErrorTexts
    miniDict (imp,df) = M.fromListWith (flip (S.><)) $ map (ent imp) (df_words df)
    ent imp (w,(ln,def)) = (w, S.singleton ((imp,ln),def))
    multiDict = M.unionsWith (S.><) $ map miniDict dfs
    overrides = M.toList (M.filter ((> 1) . S.length) multiDict)
    overrideWarnings = S.fromList (map warningOnOverride overrides)
    rawDict = M.mapMaybe lastElem multiDict 

-- last element from a sequence
lastElem :: S.Seq a -> Maybe a
lastElem s =
    case S.viewr s of
        S.EmptyR -> Nothing
        (_ S.:> e) -> Just e 


-- | cleanup dictionary to remove all words whose definitions are 
-- incomplete or part of a cycle. Also returns warnings for cycles
-- or incomplete definitions.
cleanupDictionary :: Dictionary -> (S.Seq ErrorText, Dictionary)
cleanupDictionary rawDict = (errors, cleanDict) where
    errors = cycleErrors S.>< missingWordErrors
    abstractGraph = M.map (aoWordsRequired . snd) rawDict
    cyclesDetected = detectCycles abstractGraph
    cycleErrors = S.fromList (map errorOnCycle cyclesDetected)
    cycleFreeDict = L.foldr M.delete rawDict (L.concat cyclesDetected)
    missingWordMap = incompleteWords $ M.map (aoWordsRequired . snd) cycleFreeDict
    missingWordErrors = S.fromList $ map errorOnMissing $ M.toList missingWordMap 
    cleanDict = L.foldr M.delete cycleFreeDict (M.keys missingWordMap)


-- detect cycles in an abstract directed graph 
detectCycles :: (Ord v) => M.Map v (Set v) -> [[v]]
detectCycles = deforest . M.map (Set.toList) where
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

-- find incomplete words based on missing transitive dependencies
-- the result is a map of word -> missing words.
incompleteWords :: (Ord w) => M.Map w (Set w) -> M.Map w (Set w)
incompleteWords dict = dictMW where
    (dictMW, _) = L.foldl cw (M.empty, Set.empty) (M.keys dict)
    cw r w = 
        if (M.member w (fst r) || Set.member w (snd r)) then r else
        case M.lookup w dict of
            Nothing -> r -- word without a definition
            Just deps ->
                let (dmw, dok) = L.foldl cw r (Set.toList deps) in
                let mdeps = Set.filter (`Set.notMember` dok) deps in
                if Set.null mdeps 
                    then (dmw, Set.insert w dok) 
                    else (M.insert w mdeps dmw, dok)

-- compile a clean (acyclic, fully defined) dictionary to ABC.
-- This is achieved by progressive inlining. 
compileDictionary :: Dictionary -> DictC
compileDictionary aoDict = abcDict where
    abcDict = L.foldl cw M.empty (M.keys aoDict)
    cw dc w =
        if M.member w dc then dc else
        maybe dc (cwd dc w) (M.lookup w aoDict)
    cwd dc w (_,def) =
        let deps = aoWordsRequired def in
        let dc' = L.foldl cw dc (Set.toList deps) in
        let abc = aoToABC dc' def in
        M.insert w abc dc'

-- | compile a definition to ABC, given a dictionary that
-- already contains the necessary words. Any missing words will
-- result in an annotation `{&~word}`, but it's probably better
-- to avoid the situation and test for missing words in advance.
aoToABC :: DictC -> S.Seq Action -> S.Seq Op
aoToABC dc = S.foldr (S.><) S.empty . fmap (actionToABC dc)

actionToABC :: DictC -> Action -> S.Seq Op
actionToABC dc (Word w) = maybe (annoMW w) id (M.lookup w dc)
actionToABC _ (Num r) = quoteNum r S.|> Op 'l'
actionToABC _ (Lit txt) = S.empty S.|> TL txt S.|> Op 'l'
actionToABC dc (BAO aoDef) = S.empty S.|> BL (aoToABC dc aoDef) S.|> Op 'l'
actionToABC _ (Prim ops) = ops
actionToABC dc (Amb [onlyOption]) = aoToABC dc onlyOption
actionToABC dc (Amb options) = S.singleton $ AMBC $ map (aoToABC dc) options

annoMW :: W -> S.Seq Op
annoMW = S.singleton . Invoke . T.cons '&' . T.cons '~'



-- | mangleWords will apply a function to every word in a
-- dictionary. The function should be injective (no ambiguity).
-- If the function is not injective, the dictionary may silently 
-- lose a few definitions.
--
-- This is useful if compiling to a target with a more restrictive
-- identifier model than AO's.
mangleWords :: (W -> W) -> Dictionary -> Dictionary
mangleWords rename = M.fromList . map mangleWord . M.toList where
    mangleWord (w,(loc,def)) = (rename w, (loc, mangleDef def))
    mangleDef = fmap mangleAction
    mangleAction (Word w) = Word $ rename w
    mangleAction (Amb options) = Amb $ fmap mangleDef options
    mangleAction (BAO code) = BAO $ fmap mangleAction code
    mangleAction a = a

-}
