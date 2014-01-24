{-# LANGUAGE FlexibleContexts, PatternGuards, CPP #-}

-- | Code to work with AO files...
--
module AO.AO
    (
    ) where

{-

import Control.Monad
import Control.Exception (assert)
import Control.Arrow (second)
import Data.Either (rights, lefts)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P
import Text.Parsec.Text()
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified System.Environment as Env
import qualified System.IO.Error as Err
import qualified System.IO as Sys
import AO.ABC

------------------------------
-- PROCESSING OF DICTIONARY --
------------------------------

-- | Compile an acyclic dictionary (from AO to ABC). 
-- 
-- At the moment, no typechecking is performed. The words are
-- simply expanded to ABC. Each word is expanded only once.
--
-- Errors may be reported if a word is missing.
compileDict :: Dict -> ([Error], DictC)
compileDict dict = (errors, dictC) where
    (dictMW,dictC) = L.foldl compileW (M.empty, M.empty) (M.toList dict)
    missingErrors = L.map mwError $ M.toList dictMW
    mwError (w,mws) = 
        T.pack "ERROR: word " `T.append` w `T.append` 
        T.pack " requires definitions for: " `T.append` T.unwords mws
    errors = missingErrors
    loadW w = distrib w $ maybe (Left ()) Right $ M.lookup w dict
    compileW r (w,def) = case (M.lookup w (snd r), M.lookup w (fst r)) of
        (Just _, _) -> r -- word has already been compiled
        (_, Just _) -> r -- word cannot be compiled (missing words)
        (Nothing, Nothing) -> -- attempt to compile this word
            -- first compile words used by this def into dictionary
            let reqWords = rights $ map loadW $ aoWords def in 
            let (dmw,dc) = L.foldl compileW r reqWords in
            -- compile the provided definition; use updated dictionary
            case compileAO dc def of
                Left mws -> (M.insert w (L.nub mws) dmw , dc)
                Right abcdef -> (dmw , M.insert w abcdef dc)

-- compile AO code given a dictionary containing the required
-- word definitions. If words are missing, it will return a 
-- list of those, instead.
compileAO :: DictC -> AO -> Either [W] ABC
compileAO dc (AO actions) = 
    let r0 = map (compileAction dc) actions in
    let abcOps (ABC ops) = ops in
    case lefts r0 of
        [] -> Right $ ABC $ L.concat $ map abcOps $ rights r0
        lw -> Left (L.concat lw)

-- compile a single action, given a precompiled dictionary 
-- containing all required words... or report missing words
compileAction :: DictC -> Action -> Either [W] ABC
compileAction dc (Word w []) = compileWord dc w
compileAction dc (Word w advs) =
    -- foo\adv = [foo] [\adv] applyWithAdverbs
    let cW = compileWord dc w in
    let cAdvs = compileAction dc (Adverbs advs) in
    let cAp = compileWord dc applyWithAdverbs in
    let errs = either id (const []) in
    case (cW,cAdvs,cAp) of
        (Right abcW, Right abcAdv, Right abcAp) ->
            Right $ ABC $
            (Qu (B (block abcW)) : Op 'l' : 
             Qu (B (block abcAdv)) : Op 'l' :
             inABC abcAp)
        _ -> Left $ errs cW ++ errs cAp ++ errs cAdvs
compileAction dc (Adverbs advs) = 
    -- \adverbs = \a \d \v \e \r \b \s (from dictionary)
    let cAdvs = L.map (compileWord dc . adverbWord) advs in
    case lefts cAdvs of
        [] -> Right $ ABC $ L.concatMap inABC $ rights cAdvs
        errors -> Left $ L.concat errors
compileAction _ (Num r) = Right $ ABC $ [Qu (N r), Op 'l']
compileAction _ (Lit txt) = Right $ ABC $ [Qu (toABCV txt), Op 'l']
compileAction dc (AOB ao) =
    case compileAO dc ao of
        Left mws -> Left mws -- missing word in a block
        Right abc -> Right $ ABC $ [Qu (B (block abc)), Op 'l']
compileAction dc (Amb options) =
    let r0 = map (compileAO dc) options in
    case lefts r0 of
        [] -> Right $ ABC $ [AMBC (rights r0)]
        lw -> Left (L.concat lw)
compileAction _ (Prim abc) = Right abc

compileWord :: DictC -> W -> Either [W] ABC
compileWord dc w = 
    case M.lookup w dc of
        Nothing -> Left [w]
        Just abc -> Right abc


-- buildDict centralizes a lot of processing
-- reports many errors, and removes cyclic definitions
buildDict :: [(Import,ImpR)] -> ([Error],Dict)
buildDict imps = (errors, dictCycleFree) where
    errors = importErrors -- ambiguous or missing imports
          ++ importCycleErrors -- cyclic imports
          ++ parseErrors -- bad parse entries
          ++ dupWarnings -- warn whenever a word is defined twice
          ++ defCycleErrors
    defCycles = (detectCycles . M.toList . M.map aoWords) dict
    defCycleErrors = map showCycleError defCycles
    showCycleError cyc =
        T.pack "ERROR: cycle among definitions: " `T.append`
        T.intercalate (T.pack " \x2192 ") cyc
    dictCycleFree = L.foldr M.delete dict (L.concat defCycles) -- dict without cycles
    impsD = map (uncurry distrib) imps
    importErrors = map (T.pack . show) $ lefts impsD
    goodImports = rights impsD -- [(Import, (FilePath, DictF))]
    importCycleErrors = map cycToErr $ detectCycles $ map impAdj goodImports
    impAdj (imp,(_,(impList,_))) = (imp,impList)
    cycToErr cyc = 
        T.pack "Import cycle: " `T.append` 
        T.intercalate (T.pack " \x2192 ") cyc
    parseEnts = L.concatMap parseEnt goodImports -- [Either (FP,(L,P.Err)) (FP, (L, (W, AO)))] 
    parseEnt (_, (fp, (_, ents))) = map (distrib fp . uncurry distrib) ents
    parseErrors = map showParseError (lefts parseEnts)
    showParseError (fp,(ln,err)) = T.pack (show (modErrPos (fixpos fp ln) err))
    modErrPos f e = P.setErrorPos (f (P.errorPos e)) e
    fixpos fp ln = (`P.setSourceName` (show fp)) . (`P.incSourceLine` (ln - 1))
    -- list of words, including duplicates (value is non-empty list)
    dictM = M.fromListWith (++) $ L.map toEntM (rights parseEnts)
    toEntM x@(_, (_, (w, _))) = (w, [x])
    dict = M.map (toDictEnt . L.head) dictM
    toDictEnt (fp, (l, (w, ao))) = withFrame fp l w ao
    dupWarnings = L.map toDupWarning $ M.toList $ M.filter hasDupDef dictM
    hasDupDef (_:_:_) = True
    hasDupDef _ = False
    toDupWarning (w, defs) = 
        T.pack "Word redefined: " `T.append` w `T.append` 
        T.pack " @ " `T.append` T.unwords (L.map dupLoc $ L.reverse defs)
    dupLoc (fp, (l, _)) =
        (either id id . FS.toText . FS.filename) fp 
        `T.snoc` ':' `T.append` (T.pack (show l))

-- Add location annotations to the compiled entries.
--  This adds two annotations: {&@word file line} and {&@- } to push and pop
--  frame information within the AO code. This can help with debugging.
withFrame :: FS.FilePath -> Line -> W -> AO -> AO
withFrame fp l w (AO actions) = AO (inFrame : (actions ++ [exFrame])) where 
    exFrame = Prim $ ABC [Invoke $ T.pack "&@-"]
    inFrame = Prim $ ABC [Invoke $ frameText]
    frameText = T.pack "&@" `T.append` w `T.snoc` '@' 
                `T.append` fptxt `T.snoc` ':' 
                `T.append` (T.pack (show l))
    fptxt = either id id $ FS.toText $ FS.filename fp
       
-- AO forbids recursive import lists and definitions. It is important
-- that these errors be caught to avoid infinite expansions for any
-- definitions.
--
-- This function reports cycles based on a depth first search on an
-- adjacency list. Cycles are reported as lists of linked elements, 
-- with the last item implicitly linked to the first. 
detectCycles :: (Ord v) => [(v,[v])] -> [[v]]
detectCycles = deforest . M.map (L.sort . L.nub) . M.fromListWith (++) where
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
                assert (L.elem v cx) $ -- assert visited v
                let cyc0 = L.reverse (L.take (n + 1) p) in
                let (cxw,cycw) = dfs g cx p vs in
                (cxw,(cyc0:cycw))
    edgesFrom g v = maybe [] id $ M.lookup v g

-}