

module Awelon 
    ( Word, ModuleName, LocalName
    , WX(..)
    , Module(..)
    , App(..)
    ) where

import qualified Data.Text.Lazy as T
import Control.Exception (assert)
import Data.Char
import Data.Maybe

type Text = T.Text
type Word = Text
type ModuleName = Word
type LocalName = ModuleName
type Import = (ModuleName,LocalName)
type Definition = (Word,Code)

-- Awelon has simple modules
data Module = Module
    { name        :: ModuleName
    , imports     :: [Import] 
    , definitions :: [Definition]
    }
data App = App
    { appStart    :: ModuleName
    , searchPath  :: [FilePath]
    , modules     :: [Module]
    }
data WX -- word expression
    = W  ModuleName Word 
    | LN Rational         
    | LT Text
    | LB Code
type Code = [ WX ] -- raw code

data Type               -- truly is just a compile-time value
    = Sig P V           -- runtime signal, type V in partition P
    | ST Text           -- static text
    | SN Rational       -- static number
    | SB BT Code        -- static code as a value
    | Unit              -- from intro1, identity for (*)
    | Zero              -- from intro0, identity for (+) or (|)
    | Prod Type Type    -- basic product
    | DSum Type Type    -- dynamic sum (x + y)
    | SSum Type Type    -- static sum (x | y); choice may be unknown
    | FSum Type Type    -- a 'merged' static sum
    -- EXOTIC TYPES
        -- capabilities, in general. How shall I describe these?
        -- unique source: GUIDs, sealer/unsealer pairs, exclusive state?
        -- sealed values


data BType = Block 
    { nocopy :: Maybe ExpLoc  -- affine type
    , nodrop :: Maybe ExpLoc  -- relevant type
    , code   :: Code           
    , srcid  :: Text          -- fingerprint for construction of block
    }




-- TODO: Deal with source locations for good debugging.
-- a SrcLoc has a direct physical location in source code. Though it 
-- is specified here by module, word, and index. In case of internal 
-- blocks, the index may have levels.
-- data SrcLoc = SrcLoc ModuleName Word [Int]

-- an ExpLoc describes the current 'expansion' of code to
-- reach a given word, from the initial behavior. The real
-- challenge of ExpLoc is dealing with static composition 
-- blocks. 
-- data ExpLoc = [SrcLoc]







data P = P { partition :: Text, latency :: Rational }
data V = VUnit | VText VT | VNumber VN | VBlock VB
data VT = VT { vt_const :: Maybe Text }
data VN = VN 
    { vn_min   :: Maybe Rational
    , vn_max   :: Maybe Rational
    , vn_const :: Maybe Rational
    }


-- Note: need to analyze receive/return pairs. Might use source locations?

data UniqueSrc = UniqueSrc 
    { path = [Text]         -- used children
    , children = [Text]     
    }




-- To help with source locators, first we'll treat blocks as words
-- such that a block that is the 3rd item of word `foo` might be 
-- implicitly named `foo:3` (programmers cannot use these words).
-- The composition of blocks must also be named; maybe I'll simply
-- use the combined text e.g. "foo:3 bar:2" to denote this 
-- composition.
--
-- An expansion with first-class functions must also address usage
-- context, i.e. the unique location of 'first' or 'left' (or use
-- of 'foreach', potentially). 
-- 
-- An expansion must address not only source location, but context.
-- For a block, usage context would always be the 




-- Expanded location? In an "expansion" of the code, location 
-- might be described by a path (described by data-plumbing) 
-- and depth? Or maybe an alternative is to use a fingerprinting 
-- technique for locations.

-- IDEA: Automatically strip 'blocks' out of definitions and
-- rename them to help with debugging. The name for a block
-- might be `word#pos`. 

 
-- Awelon uses its type-system for everything, i.e. static data,
-- multi-stack environment, navigating the environment, and so on.

-- I'm going to start minimal here and build up types as
-- I need them. I already know I might want:
--    sum types, dead types
--    zero types
--    error tracking; modeling errors in the type system*
--    atom types, atomic signals
--    
--    


-- get back to these...



-- code is a list of word-expressions. 
-- (Empty code is equivalent to id.)

-- | a word-expression is a word, a number, text, 
-- or a block of the same. Awelon is very easy to
-- process. 


-- PARSING MODULES
parseModuleText :: ModuleName -> Text -> Module
parseModuleText n = 
    linesToModule n .
    map parseLogicalLine . 
    extractLogicalLines .
    stripLineComments

-- remove all line comments from text. 
-- line comments in Awelon simply start with '%'.
-- NOTE: keeps the newline characters; handles text literals
stripLineComments :: Text -> Text
stripLineComments = cc T.empty where
    -- cc is just the normal state.
    cc tOut tIn = 
        case T.uncons tIn of
            Nothing -> tOut
            Just (c,tIn') ->
                let tOut' = T.snoc tOut c in
                if ('%' == c) then lc tOut tIn' else
                if ('"' /= c) then cc tOut' tIn' 
                              else tc tOut' tIn'
    -- tc, assume we're parsing text literals.
    tc tOut tIn =
        case T.uncons tIn of
            Nothing -> tOut
            Just (c,tIn') ->
                let tOut' = T.snoc tOut c in
                if ('"' == c) then cc tOut' tIn' else
                if ('\\' == c) then tce tOut' tIn'
                               else tc tOut' tIn'
    -- tce is after an escape. 
    tce tOut tIn =
        case T.uncons tIn of
            Nothing -> tOut
            Just (c,tIn') ->
                let tOut' = T.snoc tOut c in
                tc tOut' tIn'
    lc tOut tIn =
        case T.uncons tIn of
            Nothng -> tOut
            Just (c,tIn') -> 
                if ('\n' == c) then cc (T.snoc tOut '\n') tIn'
                               else lc tOut tIn'

-- break code into logical lines; does not lose any characters.
extractLogicalLines :: Text -> [Text]
extractLogicalLines = llc T.empty where
    llc tOut tIn = 
        case T.uncons tIn of
            Nothing -> (tOut:[])
            Just (c,tIn') ->
                let tOut' = T.snoc tOut c in
                if ('\n' == c) then lln tOut' tIn' 
                               else llc tOut' tIn'
    lln tOut tIn =
        case T.uncons tIn of
            Nothing -> (tOut:[])
            Just (c,tIn') ->
                if isSpace c then llc tOut tIn 
                             else tOut:(llc T.empty tIn)


type Line = Either [Import] Definition

parseLogicalLine :: Text -> Line
parseLogicalLine txt = 
    if (T.pack "import ") `isPrefixOf` txt 
        then Left $ parseImports (drop 6 txt)
        else Right $ parseDefinition txt

-- an imports list is a comma-separated list of the form:
--   foo,bar,baz, packageWithLongName AS p, etc
-- module names should be valid words, but they are not 
-- validated here.
parseImports :: Text -> [Import]
parseImports = 
    map toImport . filter (not . T.null) . 
    map strip . splitBy (== ',')
  where
    toImport txt = 
        let (mn0,ln0) = break " AS " txt in
        let ln1 = strip ln0 in
        let mnf = strip mn0 in
        let lnf = if (T.null ln1) then mnf else ln1 in
        (mnf,lnf)

-- a definition has the form:
--   word = code [blocks of code] "text" 42 12.3 numbers
parseDefinition :: Text -> Definition


        

linesToModule :: ModuleName -> [Text] -> Module
linesToModule n ls = mod where
    mod = Module n imps defs
    lsRead = map readLine ls
    readLine txt = 
        


-- appStart code interprets words as module names
--    This allows one to compile an application using text of the
--    form "[myApp] myAppFramework" or similar. 
fixAppWords :: Code -> Code
fixAppWords = map f where
    f (W mn w) = if (T.null mn) 
        then W w (T.pack "this")
        else W mn w
    f (B code) = B (fixAppCode code)
    f x = x


-- Words not starting with '_' are exported.
isExportWord :: Word -> Boolean
isExportWord w = assert ((not . S.null) w) $ ('_' /= head w)




-- cyclic modules are not allowed in Awelon.
-- returns a list of offending pairs. 
--detectCyclicModules :: App -> [(ModuleName,ModuleName)]






































