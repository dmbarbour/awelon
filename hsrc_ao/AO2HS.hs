{-# LANGUAGE PatternGuards #-}

-- | AO2HS will compile AO code into a syntax suitable for a Haskell
-- operation. However, it doesn't currently specify that interpretation.
-- Eventually, it might. As it matures. For now, however, it simply
-- requires an `AOPrelude` import be provided by the developer, with
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
    , ao2hs_mangle, ao2hs, action2hs, dict2hs
    ) where

import Data.Maybe (mapMaybe)
import Data.Ratio (numerator, denominator)
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Text as T
-- import qualified Data.List as L
-- import qualified Data.Sequence as S
import qualified Data.Foldable as S
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
        \-- This file contains the full contents of an AO dictionary, translated into\n\
        \-- Haskell for further compilation or performance. In addition, it exports\n\
        \-- an 'allWords' association list to access words as dynamic values.\n\
        \-- \n\
        \-- The client of this file should define `AOPrelude`, which will be imported\n\
        \-- as the only dependency (no implicit prelude). This prelude should define\n\
        \-- ABC primitive words, plus a few extras like `dynWord` and `>>>`. The prelude\n\
        \-- determines the types for the AO code, whether it is interpreted reactively,\n\
        \-- and so on. An example AOPrelude will provide inline documentation.\n\
        \-- \n\
        \-- Note that 'number', 'text', and 'block' explicitly include the 'op_l' that\n\
        \-- was implicit in AO. Invocations are reduced to 'seal', 'unseal', and 'anno'."
    mangledWords = fmap ao2hs_mangle $ M.keys dict
    moduleDecl = 
        T.pack   "module AODict\n    (" `T.append` 
        T.intercalate (T.pack "\n    ,") mangledWords `T.append`
        T.pack                "\n    ,allWords, module AOPrelude) where"
    importsList = T.pack "import AOPrelude"
    body = allWordsList `before` wordDefs
    allWordsList =
        T.pack      "allWords =\n    [" `T.append`
        T.intercalate (T.pack "\n    ,") (fmap wordPair (M.keys dict)) `T.append`
        T.pack                "\n    ]"
    wordPair w =
        T.pack "(\"" `T.append` w `T.append` T.pack "\", dynWord " 
        `T.append` ao2hs_mangle w `T.append` T.singleton ')'
    wordDefs = T.unlines (fmap wordDef (M.toList dict))
    wordDef (w,(loc,def)) =
        ao2hs_mangle w `T.append` T.pack " = -- " `T.append` 
        wordLocatorText w loc `T.append` T.pack "\n" 
        `T.append` indent (T.pack "  ") (ao2hs def)

indent :: Text -> Text -> Text
indent sp txt = T.unlines $ fmap (sp `T.append`) (T.lines txt)

runAO2HS :: AODef -> IO ()
runAO2HS = Sys.putStrLn . T.unpack . ao2hs 

ao2hs :: AODef -> Text
ao2hs actions = 
    let hsActions = mapMaybe action2hs (S.toList actions) in
    if null hsActions then T.pack "pass" else
    T.intercalate (T.pack " >>> ") hsActions

action2hs :: Action -> Maybe Text
action2hs (Word w) = Just $ ao2hs_mangle w
action2hs (Num r) = Just $ T.pack (num2hs r) `T.append` T.pack " >>> op_l"
action2hs (Lit txt) = Just $ T.pack "text " 
    `T.append` T.pack (show txt) `T.append` T.pack " >>> op_l" 
action2hs (BAO def) = Just $ T.pack "block (" 
    `T.append` ao2hs def `T.append` T.pack ") >>> op_l"
action2hs (Amb [singleton]) = Just $ '(' `T.cons` ao2hs singleton `T.snoc` ')'
action2hs (Amb _options) = error "ambiguous code not yet supported"
action2hs (Prim ops) =
    let hsOps = ops2hs (S.toList ops) in
    if null hsOps then Nothing else
    Just $ T.intercalate (T.pack " >>> ") hsOps

num2hs :: Rational -> String
num2hs n | ((n >= 0) && (1 == denominator n)) = "number " ++ show (numerator n)
num2hs n | (1 == denominator n) = "number (" ++ show (numerator n) ++ ")"
num2hs r = "number (" ++ show r ++ ")"

-- ops2hs performs a few simplifications as it goes:
--   eliminate ABC whitespace (identity behavior)
--   recognize '$c' which potentially allows tail-call optimization
-- This doesn't perform deeper simplifications that would cross words.
ops2hs :: [Op] -> [Text]
ops2hs [] = []
ops2hs (Op ' ' : ops) = ops2hs ops
ops2hs (Op '\n' : ops) = ops2hs ops
ops2hs (Op '$' : Op 'c' : ops) = (T.pack "ops_apc") : ops2hs ops
ops2hs (Op c : ops) = opc2hs c : ops2hs ops
ops2hs ((Invoke txt) : ops) = invocation2hs txt : ops2hs ops
ops2hs ops = error ("illegal inline ABC: " ++ show ops)

invocation2hs :: Text -> Text
invocation2hs txt = case T.uncons txt of
    Just ('&',anno) -> T.pack "anno " `T.append` T.pack (show anno)
    Just (':',sealer) -> T.pack "seal " `T.append` T.pack (show sealer)
    Just ('.',sealer) -> T.pack "unseal " `T.append` T.pack (show sealer)
    _ -> error ("unrecognized AO invocation: " ++ show txt)

opc2hs :: Char -> Text
opc2hs 'l' = T.pack "op_l"
opc2hs 'r' = T.pack "op_r"
opc2hs 'w' = T.pack "op_w"
opc2hs 'z' = T.pack "op_z"
opc2hs 'v' = T.pack "op_v"
opc2hs 'c' = T.pack "op_c"
opc2hs 'L' = T.pack "op_L"
opc2hs 'R' = T.pack "op_R"
opc2hs 'W' = T.pack "op_W"
opc2hs 'Z' = T.pack "op_Z"
opc2hs 'V' = T.pack "op_V"
opc2hs 'C' = T.pack "op_C"
opc2hs '%' = T.pack "op_drop"
opc2hs '^' = T.pack "op_copy"
opc2hs '$' = T.pack "op_ap"
opc2hs '\'' = T.pack "op_quote"
opc2hs 'o' = T.pack "op_comp"
opc2hs 'k' = T.pack "op_rel"
opc2hs 'f' = T.pack "op_aff"
opc2hs '+' = T.pack "op_add"
opc2hs '*' = T.pack "op_mul"
opc2hs '-' = T.pack "op_neg"
opc2hs '/' = T.pack "op_inv"
opc2hs 'Q' = T.pack "op_div"
opc2hs '?' = T.pack "op_condap"
opc2hs 'D' = T.pack "op_distrib"
opc2hs 'F' = T.pack "op_factor"
opc2hs 'M' = T.pack "op_merge"
opc2hs 'K' = T.pack "op_assert"
opc2hs '>' = T.pack "op_gt"
opc2hs c = error ("unrecognized inline ABC operation: " ++ [c]) 


