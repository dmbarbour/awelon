
-- This file provides the main AST and parsers for an AO
-- dictionary file. It does not process AO, beyond parsing it.
-- (Which is to say, it does not detect cycles, missing words,
-- or other problems.) It also doesn't load imports recursively.
module AO.ParseAO
    ( readDictFileText
    -- PARSERS
    , parseImportEntry, parseDefEntry, parseAOCode
    , parseWord, parseFullWord
    , parseNumber, parseAction
    , parseMultiLineText, parseInlineText
    -- ABSTRACT SYNTAX
    , DictFile(..), Action(..), Import(..), Entry(..)
    ) where

import Data.Ratio
import qualified Data.Sequence as S

type W = Text
type M = Char
data Action 
    = Word W [M] -- 'word' or 'word\adverbs' (allowing inflection)
    | Adverbs [M] -- '\adverbs' expands to '\a \d \v \e \r \b \s'
    | Num Rational -- literal number, many formats accepted
    | Lit Text -- text literal (inline or multi-line)
    | BAO (S.Seq Action) -- block of AO code
    | Prim (S.Seq Op) -- %inlineABC
    | 
    




type DictF = ([Import],[(Line, ParseEnt)]) -- one dictionary file
type ParseEnt = Either P.ParseError (W,AO) -- one parsed entry (or error)
type Import = Text -- name of dictionary file (minus '.ao' file extension)
type Entry = Text  -- text of entry within a dictionary file
type Line = Int    -- line number for an entry
type W = Text      -- basic word
type ADV = Char    -- adverb
data Action 
    = Word W [ADV]  -- word\adverbs
    | Num Rational  -- literal number
    | Lit Text      -- literal text
    | AOB AO        -- block of AO
    | Amb [AO]      -- ambiguous choice of AO (non-empty)
    | Prim ABC      -- %inlineABC
    | Adverbs [ADV] -- \adverbs
    deriving Show
newtype AO = AO [Action] deriving Show
type Error = Text
type Dict = M.Map W AO
type DictC = M.Map W ABC