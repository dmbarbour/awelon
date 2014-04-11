
-- | pure description of AO code plus useful 'show' operations
--
-- Note: the AO parser preserves spaces (encoding Op_SP and Op_LF).
--  These are mostly preserved on 'show', though a space or newline
--  may be injected in a few cases, idempotently. Number formats 
--  aren't preserved. 'show' generates code suitable for parsing.
--
module AO.Code
    ( AO_Action(..), Word, AO_Code
    ) where

import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import ABC.Operators (OpC(..))

-- | Word is Text (to support fast comparisons)
type Word = Text

-- | AO code is simply a list of actions
-- (usually, this is a very short list)
type AO_Code = [AO_Action]

-- | an AO operation
data AO_Action 
    = AO_Word Word
    | AO_Block [AO_Action]
    | AO_Num Rational
    | AO_Txt String
    | AO_ABC OpC
    | AO_Tok String    -- {token}
    deriving (Eq, Ord)

instance Show AO_Action where
    showsPrec _ op = showList [op]        -- always display as a list
    showList ops@(AO_Txt _ : _) = sSP ops -- safe inline vs. multi-line text
    showList ops = sa False ops           -- nothing else is space sensitive

type AtNewLine = Bool

sa :: AtNewLine -> [AO_Action] -> ShowS
sa _ (AO_Word w : more) = showString (T.unpack w) . sWithSP more
sa _ (AO_ABC Op_SP : more) = sSP more -- consume SP
sa _ (AO_ABC Op_LF : more) = sLF more -- consume LF
sa _ [] = id -- all done
sa _ (AO_Block ops : more) =
    showChar '[' . sa False ops . showChar ']' . sWithSP more
sa False (AO_Txt s : more) | inlineableTxt s = 
    showChar '"' . showString s . showChar '"' . sWithSP more
sa bAtNL (AO_Txt s : more) = 
    let enterNL = if bAtNL then id else showChar '\n' in
    enterNL . showChar '"' . showEscaped s . 
    showChar '\n' . showChar '~' . sWithSP more
sa _ (AO_Num r : more) = showNumber r . sWithSP more
sa _ (AO_ABC c0 : ops) =
    let (cs,more) = gatherABC [] ops in
    showChar '%' . showList (c0 : cs) . sWithSP more
sa _ (AO_Tok s : more) = 
    showChar '{' . showString s . showChar '}' . sWithSP more

-- gather as much ABC code as possible, excluding spaces
gatherABC :: [OpC] -> [AO_Action] -> ([OpC],[AO_Action])
gatherABC cs (AO_ABC c : more) | not (isSP c) = gatherABC (c:cs) more
gatherABC cs actions = (L.reverse cs, actions)

isSP :: OpC -> Bool
isSP Op_SP = True
isSP Op_LF = True
isSP _ = False

-- escape a string for AO's multi-line text 
showEscaped :: String -> ShowS
showEscaped ('\n':s) = showChar '\n' . showChar ' ' . showEscaped s
showEscaped (c:s) = showChar c . showEscaped s
showEscaped [] = id

inlineableTxt :: String -> Bool
inlineableTxt = L.all inlinableChr

-- characters '"' and '\n' may not be inlined
inlinableChr :: Char -> Bool
inlinableChr c = not ('\n' == c || '"' == c)

sSP,sLF,sWithSP :: [AO_Action] -> ShowS
sSP ops = showChar ' ' . sa False ops
sLF ops = showChar '\n' . sa True ops
sWithSP (AO_ABC Op_SP : more) = sSP more -- use preserved SP
sWithSP (AO_ABC Op_LF : more) = sLF more -- use preserved LF
sWithSP [] = id -- no need to add space (end of block or code)
sWithSP ops = sSP ops -- inject a space

-- trivial showNumber
--  note: I'd like to switch to decimal display in some cases
showNumber :: Rational -> ShowS
showNumber r | (1 == denominator r) = shows (numerator r) 
             | otherwise = shows (numerator r) . showChar '/' . shows (denominator r)
