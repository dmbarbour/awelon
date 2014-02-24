
-- basic data types, abstract syntax, simple processing for AO
module AO.AOTypes 
    ( W, ADV, Action(..), Import
    , AODef, Dictionary
    , Line, Locator, DictFile(..)
    , applyWithAdverbs
    , aoWordsRequired
    , locatorText, wordLocatorText
    , mangleWords
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
    | Amb [AODef] -- ambiguous choice, or maybe just one choice
type Import = Text
type Line = Int
type Locator = (Import,Line) 
type AODef = S.Seq Action
type Dictionary = M.Map W (Locator, AODef)

-- a parsed dictionary file
data DictFile = DictFile
    { df_imports :: [Import]
    , df_words   :: [(W,(Line,AODef))]
    , df_errors  :: [(Line,Text)]
    }

-- applyWithAdverbs is a word that is used by the syntactic sugar
-- for inflection in AO. A word of the form `foo\*kd` is expanded
-- to `[foo] [\*kd] applyWithAdverbs`. The code `\*kd` is further
-- expanded to sequence of special words `\* \k \d`. 
--
-- this syntactic sugar is currently handled by the parser, such
-- that it does not appear in the AST. 
applyWithAdverbs :: W
applyWithAdverbs = T.pack "applyWithAdverbs"


-- | find the words required for a given word
aoWordsRequired :: AODef -> Set W
aoWordsRequired = Set.unions . S.toList . fmap actionWordsRequired

-- words needed for a single action
actionWordsRequired :: Action -> Set W
actionWordsRequired (Word w) = Set.singleton w
actionWordsRequired (BAO actions) = aoWordsRequired actions
actionWordsRequired (Amb options) = Set.unions (map aoWordsRequired options)
actionWordsRequired _ = Set.empty

-- bar:42 (bar is import name)
locatorText :: Locator -> Text
locatorText (imp,ln) = imp `T.snoc` ':' `T.append` T.pack (show ln)

-- foo@bar:42
wordLocatorText :: W -> Locator -> Text
wordLocatorText w loc = w `T.snoc` '@' `T.append` locatorText loc

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
