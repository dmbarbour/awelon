

module Awelon 
    ( Word, ModuleName, LocalName
    , WX(..)
    , Module(..)
    , App(..)
    ) where

import qualified Data.Text.Lazy as T
import Control.Exception (assert)
import Data.Char 

type Text = T.Text
type Word = Text
type ModuleName = Word
type LocalName = ModuleName

-- code is a list of word-expressions. 
-- (Empty code is equivalent to id.)
type Code = [ WX ]

-- | a word-expression is a word, a number, text, 
-- or a block of the same. Awelon is very easy to
-- process. 
data WX 
    = W  ModuleName Word
    | LN Rational
    | LT Text
    | B  Code

-- Awelon modules are simple!
data Module = 
    { imports :: [ (ModuleName, LocalName) ] 
    , definitions :: [ (Word, Code) ]  
    }

-- | A full app definition. 
--
-- Often, an app is just the name of a module, e.g. `foo` refers to
-- `foo:this`. However, developers might want to represent apps with
-- an alternative app-model, in which case an app might look closer 
-- to:  `[myApp] myAppFramework`.
-- 
-- So I'm allowing a line of code, where words are implicitly treated
-- as module names unless specified with a colon.
-- 
data App = 
    { appStart :: Code -- implicit imports here
    , modules :: [(ModuleName, Module)]
    -- , avm -- get back to this
    -- , avmprim -- get back to this
    }





-- appStart code interprets words as module names
fixAppCode :: Code -> Code
fixAppCode = map f where
    f (W mn w) = if (T.null mn) 
        then W w (T.pack "this")
        else W mn w
    f (B code) = B (fixAppCode code)
    f x = x


-- cyclic modules are not allowed in Awelon.
-- returns a list of offending pairs. 
detectCyclicModules :: App -> [(ModuleName,ModuleName)]













-- Words not starting with '_' are exported.
isExportWord :: Word -> Boolean
isExportWord w = assert ((not . S.null) w) $ ('_' /= head w)



















---------------------
-- PARSING SUPPORT --
---------------------

-- read Awelon's logical lines. A logical line is simply a line
-- that starts with a non whitespace character, and it continues
-- to the next line that does the same. No characters are lost.
-- (Note: a comment can start a logical line.)
logicalLines :: Text -> [Text]
logicalLines = llc T.empty where
    llc line text =
        case T.uncons text of
            Nothing -> line:[]
            Just (c,t) ->
                let line' = T.snoc line c in
                if (c /= '\n') then llc line' t 
                               else lln line' t
    lln line text =
        case T.uncons text of
            Nothing -> line:[]
            Just (c,t) ->
                if isSpace c then llc (T.snoc line c) t
                             else line:(llc T.empty text)

-- remove line comments and junk
stripLLs


-- remove a line-comment, if there is one. But leave the
-- newline character. 
stripLC :: Text -> Text
stripLC t0 = tf where
    tf = case T.uncons t0 of
            Nothing -> t0
            Just (c,t) -> if '-' == c then lc1 t else t0
    lc1 t = case T.uncons t of
            Nothing -> t0
            Just (c1,t1) -> if '-' == c1 then lc2 t1 else t0
    lc2 t = case T.uncons t of
            Nothing -> T.empty
            Just (c2,_) -> if isSpace c2 then lcX t else t0
    lcX t = case T.uncons t of
            Nothing -> T.empty
            Just (cx,tx) -> if '\n' == c3 then t else lcX tx


ll0 where
    


-- stripws will remove spaces and Awelon's line comments until a
-- word is found.
stripws :: Text -> Text
stripws t0 = tf where
    tf = case T.uncons t0 of
            Nothing -> t0
            Just (c,t) ->
                if isSpace c then stripws t else
                if '-' /= c then t0 else -- done with whitespace
                lc1 t -- potential line comment
    lc1 t = case T.uncons t of
            Nothing -> t0 -- ending in '-'
            Just (c1,t1) ->
                if '-' /= c1 then t0 else
                lc2 t1 -- potential line comment
    lc2 t = case T.uncons t of
            Nothing -> T.empty -- file ends right at line comment
            Just (c2,t2) ->
                if isSpace c2 then rmlc t 
                else t0 -- word prefixed with '--'. 
    rmlc t = case T.uncons t of
            Nothing -> T.empty 
            Just (cx,tx) ->
                if '\n' == cx then stripws tx else
                rmlc tx

-- break a module into a set of lines
logicalLines :: Text -> [Text]
logicalLines t0 = 
    let (line,tf) = oneLogicalLine t0 in
    if (T.null line) then assert (T.null tf) [] 
                     else line:(logicalLines tf)

-- read one logical line from text, return rest of text
oneLogicalLine :: Text -> (Text,Text)
oneLogicalLine = ll1 (T.empty)

 where
    ll t = let (line,t') = oneLine

-- readWord




-- process source code into Awelon's "logical lines". A logical line
-- begins on any line that starts without whitespace, and continues
-- to the next line that does the same. 
logicalLines :: Text -> [Text]
logicalLines t = nl where
    nl t = 
        case T.uncons 
logicalLines = reverse ((nl t):[]) where
    -- nl assumes we're looking for the start of new logical line.
    nl t = 
    ll0 t = 
    case uncons t of
        















