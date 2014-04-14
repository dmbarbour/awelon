
-- | pure description of AO code plus useful 'show' operations
--
module AO.Code
    ( AO_Action(..), AOp(..), Word, AO_Code
    , aopCharList
    , aoWords
    ) where

import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L

-- | Word is Text (to support fast comparisons)
type Word = Text

-- | AO code is simply a list of actions
-- (usually, this is a very short list)
type AO_Code = [AO_Action]

-- | all primitive AO elements (literals, words, etc.)
--
-- This does not preserve spaces, which are not considered a formal
-- element of the AO code (unlike ABC, where spaces are formally 
-- identity operations). 
--
data AO_Action 
    = AO_Word Word
    | AO_Block [AO_Action]
    | AO_Num Rational
    | AO_Text String -- literal text (inline or multi-line)
    | AO_ABC AOp -- inline ABC (in canonical form: %v %r %w %l %c)
    | AO_Tok String -- {token}
    deriving (Eq, Ord)

-- | AOp encodes just the subset of ABC that AO may inline.
data AOp
    = AOp_l | AOp_r | AOp_w | AOp_z | AOp_v | AOp_c -- basic data plumbing
    | AOp_L | AOp_R | AOp_W | AOp_Z | AOp_V | AOp_C -- sum-type data plumbing
    | AOp_copy | AOp_drop -- '^' and '%'
    | AOp_add | AOp_neg | AOp_mul | AOp_inv | AOp_divMod -- basic math
    | AOp_ap | AOp_cond | AOp_quote | AOp_comp -- higher order programming
    | AOp_rel | AOp_aff -- substructural types
    | AOp_distrib | AOp_factor | AOp_merge | AOp_assert -- working with sums
    | AOp_gt -- value observations
    deriving (Eq, Ord)

-- | table of associations between inline ABC and characters.
aopCharList :: [(AOp, Char)]
aopCharList =
    [(AOp_l,'l'),(AOp_r,'r'),(AOp_w,'w'),(AOp_z,'z'),(AOp_v,'v'),(AOp_c,'c')
    ,(AOp_L,'L'),(AOp_R,'R'),(AOp_W,'W'),(AOp_Z,'Z'),(AOp_V,'V'),(AOp_C,'C')
    ,(AOp_copy,'^'),(AOp_drop,'%')
    ,(AOp_add,'+'),(AOp_neg,'-'),(AOp_mul,'*'),(AOp_inv,'/'),(AOp_divMod,'Q')
    ,(AOp_ap,'$'),(AOp_cond,'?'),(AOp_quote,'\''),(AOp_comp,'o')
    ,(AOp_rel,'k'),(AOp_aff,'f')
    ,(AOp_distrib,'D'),(AOp_factor,'F'),(AOp_merge,'M'),(AOp_assert,'K')
    ,(AOp_gt,'>')
    ]

instance Show AO_Action where
    showsPrec _ op = showList [op]

    -- add a space prior to text due to space sensitivity
    showList ops@(AO_Text _ : _) = showChar ' ' . sa ops
    showList ops = sa ops -- otherwise not space sensitive

instance Show AOp where
    showsPrec _ op = case L.lookup op aopCharList of
        Nothing -> error "aopCharList is missing an operator!"
        Just c -> showChar c
    showList (op:ops) = shows op . showList ops
    showList [] = id

-- sa assumes that the previous character is a space or other valid
-- separator, but not necessarily a newline. Inline text will always
-- create a newline.
sa :: [AO_Action] -> ShowS
sa [] = id -- all done
sa (AO_Word w : more) = showString (T.unpack w) . sWithSP more
sa (AO_Block ops : more) =
    showChar '[' . sa ops . showChar ']' . sWithSP more
sa (AO_Text s : more) | inlineableTxt s = 
    showChar '"' . showString s . showChar '"' . sWithSP more
sa (AO_Text s : more) = 
    showChar '\n' . showChar '"' . showEscaped s . 
    showChar '\n' . showChar '~' . sWithSP more
sa (AO_Num r : more) = showNumber r . sWithSP more
sa (AO_ABC c0 : ops) =
    let (cs,more) = gatherABC [] ops in
    showChar '%' . showList (c0 : cs) . sWithSP more
sa (AO_Tok s : more) =
    showChar '{' . showString s . showChar '}' . sWithSP more

-- add a space before showing the next action
sWithSP :: [AO_Action] -> ShowS
sWithSP [] = id -- no need to add space (end of block or code)
sWithSP ops = showChar ' ' . sa ops

-- gather as much ABC code as possible, excluding spaces and tokens
gatherABC :: [AOp] -> [AO_Action] -> ([AOp],[AO_Action])
gatherABC cs (AO_ABC c : more) = gatherABC (c:cs) more
gatherABC cs actions = (L.reverse cs, actions)

-- escape a string for AO's multi-line text 
showEscaped :: String -> ShowS
showEscaped ('\n':s) = showChar '\n' . showChar ' ' . showEscaped s
showEscaped (c:s) = showChar c . showEscaped s
showEscaped [] = id

inlineableTxt :: String -> Bool
inlineableTxt = L.all inlinableChr

-- characters '"' and '\n' may not be part of inline text
inlinableChr :: Char -> Bool
inlinableChr c = not ('\n' == c || '"' == c)

-- trivial showNumber
--  note: I'd like to switch to decimal display in some cases
showNumber :: Rational -> ShowS
showNumber r | (1 == denominator r) = shows (numerator r) 
             | otherwise = shows (numerator r) . showChar '/' . shows (denominator r)


-- | Extract the words used by AO code. For example:
--     foo "hello" [42 bar baz] %vrwlc bar → [foo,bar,baz,bar]
-- Duplicates are still part of the list at this point.
aoWords :: [AO_Action] -> [Word]
aoWords = flip lw [] where
    lw (x:xs) = ew x . lw xs
    lw [] = id
    ew (AO_Word w) = (w:)
    ew (AO_Block ops) = lw ops
    ew _ = id




