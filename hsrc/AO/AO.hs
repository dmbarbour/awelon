{-# LANGUAGE FlexibleContexts, PatternGuards, CPP #-}

-- | This is the main module for working with AO. The functions here
-- are organized around loading dictionaries and working with them
-- wholistically. 
--
-- Developers can load a full dictionary, and process it to eliminate
-- bad words (those possessing cyclic or referencing missing words).
-- From there, they can compile it one of two ways:
--
--    compile each word to ABC
--    compile the AO dictionary to a Haskell text file
--
-- Or, at least that's the intention. Currently, only the compile to
-- ABC is performed. 
--
module AO.AO
    ( loadDictionary, cleanupDictionary -- load and cleanup
    , DictC, compileDictionary, aoToABC -- compilers
    , module AO.V
    , module AO.AOTypes
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as L
import qualified System.IO as Sys
import AO.V
import AO.AOTypes
import AO.LoadAO

type DictC = M.Map W (S.Seq Op) -- ABC compiled dictionary
type ErrorText = Text

reportError :: String -> IO ()
reportError [] = return ()
reportError s = Sys.hPutStrLn Sys.stderr s

-- | load dictionary from filesystem
--
-- Reports errors or warnings to stderr, but continues through them.
-- Will remove words with cyclic or incomplete definitions, so the
-- resulting dictionary is clean of those issues (though type errors
-- will remain). 
loadDictionary :: Import -> IO Dictionary
loadDictionary imp0 =
    importDictFiles imp0 >>= \ dfs ->
    let (issues, dict) = buildDictionary dfs in
    S.mapM_ (reportError . T.unpack) issues >>
    return dict

-- glue a raw dictionary together, then remove cycles and incomplete
-- words, resulting in a complete and acyclic subset of the dictionary.
buildDictionary :: [(Import,DictFile)] -> (S.Seq ErrorText, Dictionary)
buildDictionary dfs = 
    let (e1, rawDict) = buildRawDictionary dfs in
    let (e2, cleanDict) = cleanupDictionary rawDict in
    (e1 S.>< e2, cleanDict)

-- raw dictionary from files; warns for parse errors and overrides
buildRawDictionary :: [(Import, DictFile)] -> (S.Seq ErrorText, Dictionary)
buildRawDictionary dfs = (errors, rawDict) where
    errors = parseErrors S.>< overrideWarnings
    dfsErrorTexts = map (S.fromList . map snd . df_errors . snd) dfs
    parseErrors = foldr (S.><) S.empty dfsErrorTexts
    miniDict (imp,df) = M.map (S.singleton . withLoc imp) (df_words df)
    withLoc imp (ln,def) = ((imp,ln),def)
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

warningOnOverride :: (W, S.Seq (Locator, AODef)) -> ErrorText
warningOnOverride (w,defs) = 
    let locations = S.toList $ fmap (locatorText . fst) defs in
    w `T.append` T.pack " redefined @ " `T.append`
    T.unwords locations


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

errorOnCycle :: [W] -> ErrorText
errorOnCycle wordsInCycle = 
    T.pack "Error: cyclic definitions: " `T.append`
    T.unwords wordsInCycle

errorOnMissing :: (W, Set W) -> ErrorText
errorOnMissing (w,mw) =
    T.pack "Error: word " `T.append` w `T.append` 
    T.pack " needs definitions for: " `T.append`
    T.unwords (Set.toList mw)

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
        let deps = M.findWithDefault Set.empty w dict in
        let (dmw, dok) = L.foldl cw r (Set.toList deps) in
        let mdeps = Set.filter (`Set.notMember` dok) deps in
        if Set.null mdeps 
            then (dmw, Set.insert w dok) 
            else (M.insert w mdeps dmw, dok)

-- compile a clean (acyclic, fully defined) dictionary to ABC.
-- This is achieved by progressive inlining. 
--
-- This will also wrap each word in the dictionary with locators.
compileDictionary :: Dictionary -> DictC
compileDictionary aoDict = abcDict where
    abcDict = L.foldl cw M.empty (M.keys aoDict)
    cw dc w =
        if M.member w dc then dc else
        maybe dc (cwd dc w) (M.lookup w aoDict)
    cwd dc w (loc,def) =
        let deps = aoWordsRequired def in
        let dc' = L.foldl cw dc (Set.toList deps) in
        let def' = frameWrap (wordLocatorText w loc) def in
        let abc = aoToABC dc' def' in
        M.insert w abc dc'

frameWrap :: Text -> S.Seq Action -> S.Seq Action
frameWrap txt actions = enterFrame S.<| (actions S.|> exitFrame) where
    annoLoc = Prim . S.singleton . Invoke . T.cons '&' . T.cons '@'
    enterFrame = annoLoc txt
    exitFrame = annoLoc (T.singleton '-') 

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
