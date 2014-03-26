{-# LANGUAGE PatternGuards #-}

-- | AO2HS will compile AO code into a syntax suitable for a Haskell
-- operation. However, it doesn't currently specify that interpretation.
-- Eventually, it might. As it matures. For now, however, it simply
-- requires an `AO.Prelude` import be provided by the developer, with
-- an appropriate set of primitives (and ideally some nice rewrite rules).
-- 
-- The `runDict2HS` command will emit Haskell code for the full dictionary,
-- minus the AO prelude (which is user provided). This dictionary has minimal
-- dependencies, and minimal sophistication. In addition to exporting each
-- word, it exports a list of all words with their original text identifiers
-- and Haskell values.
-- 
-- The `runAO2HS` will emit Haskell code for a command, assuming the 
-- dictionary is already available and no words are missing.
--
module AO2HS
    ( runAO2HS, runDict2HS
    , ao2hs_mangle, ao2hs, dict2hs
    ) where

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import qualified System.IO as Sys
import AO.AO

-- mangle a word for use in Haskell
ao2hs_mangle :: W -> W
ao2hs_mangle word = 'w' `T.cons` '_' `T.cons` T.concatMap mangleC word where
    mangleC c | C.isAlphaNum c = T.singleton c
    mangleC '.' = T.pack "_dot_"
    mangleC '+' = T.pack "_plus_"
    mangleC '*' = T.pack "_star_"
    mangleC '-' = T.pack "_dash_"
    mangleC '/' = T.pack "_slash_"
    mangleC '\\' = T.pack "_bslash_"
    mangleC '_' = T.pack "_under_"
    mangleC '!' = T.pack "_bang_"
    mangleC '?' = T.pack "_interro_"
    mangleC c = '_' `T.cons` cp (C.ord c) `T.snoc` '_'
    cp n | n < 1 = T.empty
         | otherwise = let (q,r) = divMod n 16 in cp q `T.snoc` C.intToDigit r

runDict2HS :: IO ()
runDict2HS = 
    loadDictionary >>= \ dictAO ->
    let hsCode = dict2hs dictAO in
    Sys.writeFile "AODict.hs" (T.unpack hsCode) >>
    Sys.putStrLn "wrote active AO dictionary to AODict.hs"

dict2hs :: Dictionary -> Text
dict2hs dict = prefix `before` body where
    mdict = mangleWords ao2hs_mangle dict
    before x y = (x `T.snoc` '\n' `T.snoc` '\n') `T.append` y
    prefix = lang `before` opts `before` openingComment 
             `before` moduleDecl `before` importsList
    lang = T.pack "{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}"
    opts = T.pack "{-# OPTIONS_GHC -fno-warn-missing-signatures #-}"
    openingComment = T.pack $
        "-- NOTE: This code was automatically generated. Use `ao dict2hs` to\n\
        \-- regenerate it, assuming the ao package is installed and configured.\n\
        \-- Avoid direct modification; if a fix is needed, fix the ao package.\n\
        \-- \n\
        \-- This file contains the contents of an AO dictionary, translated into\n\
        \-- Haskell for further compilation or performance. In addition, it exports\n\
        \-- an 'allWords' association list to access words as dynamic values.\n\
        \-- \n\
        \-- The client of this file should define `AOPrelude`, which will be imported\n\
        \-- as the only dependency (no implicit prelude). This prelude should define\n\
        \-- some words, such as `dynWord` (used by `allWords`) and `abcOp_v` (and all\n\
        \-- the ABC primitives), and various types for number and text literals. An\n\
        \-- example AOPrelude, with inline documentation, should be available."
    moduleDecl = 
        T.pack  "module AODict\n    (" `T.append` 
        T.intercalate (T.pack "\n    ,") (M.keys mdict) `T.append`
        T.pack                "\n    ,allWords) where"
    importsList = T.pack "import AOPrelude"
    body = allWordsList `before` wordDefs
    allWordsList =
        T.pack      "allWords =\n    [" `T.append`
        T.intercalate (T.pack "\n    ,") (fmap wordPair (M.keys dict)) `T.append`
        T.pack                "\n    ]"
    wordPair w =
        T.pack "(\"" `T.append` w `T.append` T.pack "\", dynWord " 
        `T.append` ao2hs_mangle w `T.append` T.singleton ')'
    wordDefs = T.unlines (fmap wordDef (M.toList mdict))
    wordDef (w,(loc,def)) =
        w `T.append` T.pack " = -- " `T.append` 
        locatorText loc `T.append` T.pack "\n" 
        `T.append` indent (T.pack "  ") (ao2hs def)

indent :: Text -> Text -> Text
indent sp txt = T.unlines $ fmap (sp `T.append`) (T.lines txt)

ao2hs :: AODef -> Text
ao2hs _actions = T.pack "error \"TODO!\""
     
runAO2HS :: AODef -> IO ()
runAO2HS = Sys.putStrLn . T.unpack . ao2hs 

