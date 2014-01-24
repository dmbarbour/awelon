
-- basic data types, abstract syntax, simple processing for AO
module AO.AOTypes 
    ( W, ADV, Action(..)
    , Locator, AODef, Dictionary
    , applyWithAdverbs
    , aoWordsRequired
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import AO.V

type W = Text -- text associated with a word
type ADV = Char -- character used as adverb
data Action 
    = Word W -- a plain old word
    | Num Rational -- literal number, many formats accepted
    | Lit Text -- text literal (inline or multi-line)
    | BAO AODef -- block of AO code
    | Prim (S.Seq Op) -- %inlineABC
    | Amb [AODef] -- ambiguous choice or inlined singleton
type Locator = (Text,Int) -- (file,line) 
type AODef = S.Seq Action
type Dictionary = M.Map W (Locator, AODef)

-- applyWithAdverbs is a word that is used by the syntactic sugar
-- for inflection in AO. A word of the form `foo\*kd` is expanded
-- to `[foo] [\*kd] applyWithAdverbs`. The code `\*kd` is further
-- expanded to sequence of special words `\* \k \d`. 
--
-- this syntactic sugar is currently handled by the parser, such
-- that it does not appear in the AST. 
applyWithAdverbs :: W
applyWithAdverbs = T.pack "applyWithAdverbs"

aoWordsRequired :: AODef -> Set W
aoWordsRequired = Set.unions . S.toList . fmap actionWordsRequired

actionWordsRequired :: Action -> Set W
actionWordsRequired (Word w) = Set.singleton w
actionWordsRequired (BAO actions) = aoWordsRequired actions
actionWordsRequired (Amb options) = Set.unions (map aoWordsRequired options)
actionWordsRequired _ = Set.empty



{-
type DictF = ([Import],[(Line, ParseEnt)]) -- one dictionary file
type ParseEnt = Either P.ParseError (W,AO) -- one parsed entry (or error)
type Import = Text -- name of dictionary file (minus '.ao' file extension)
type Entry = Text  -- text of entry within a dictionary file
type Line = Int    -- line number for an entry
type W = Text      -- basic word
type ADV = Char    -- adverb
type Error = Text
type Dict = M.Map W AO
type DictC = M.Map W ABC
-}