{-# LANGUAGE PatternGuards #-}
-- | A simple tyepecheck and type annotator for ABC code. 
--
-- This is the one good thing that came from my early, simplistic
-- attempts to compile ABC code. 
--
-- This operation does perform a minimal degree of partial
-- evaluation to help check '$' and '?' applications.
--
module AO.TypeABC 
    ( Ty(..)
    , valToTy, tyToVal
    , typeOfABC 
    ) where

import Data.Monoid (mappend)
import Control.Arrow (left)
import Control.Applicative
import Control.Monad ((>=>))
import Data.Functor.Identity
import Control.Monad.Trans.Error
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text (Text)

import AO.V

data Ty
    = TyDyn
    | TyNum (Maybe Rational)
    | TyBlock KF (Maybe (S.Seq Op))
    | TyProd Ty Ty
    | TyUnit
    | TySum STy STy
    | TySeal Text Ty
type STy = (Bool, Ty)
--newtype TyX = TyX Ty -- type expected

instance Show Ty where show = summaryTy 12

summaryTy :: Int -> Ty -> String
summaryTy n _ | (n < 0) = "..."
summaryTy _ (TyDyn) = "?"
summaryTy _ (TyNum (Just r)) = show r
summaryTy _ (TyNum Nothing) = "N"
summaryTy _ (TyBlock kf mbOps) = (addDot . addF . addK) "B" where
    addK = if may_drop kf then id else (++ "k")
    addF = if may_copy kf then id else (++ "f")
    addDot = maybe (++ ".") (const id) mbOps
summaryTy n v@(TyProd a b) =
    case summaryText 14 v of
        Just txt -> T.unpack $ '"' `T.cons` txt `T.snoc` '"'
        Nothing -> 
            let msga = summaryTy (n - 1) a in
            let msgb = if (n < 1) then "..." else summaryTy (n - 1) b in
            "(" ++ msga ++ "*" ++ msgb ++ ")"
summaryTy n (TySum a b) =
    let msga = summarySTy (n-1) a in
    let msgb = if (n < 1) then "..." else summarySTy (n-1) b in
    "(" ++ msga ++ "+" ++ msgb ++ ")"
summaryTy _ TyUnit = "unit"
summaryTy n (TySeal tok v) = T.unpack tok ++ ":" ++ summaryTy n v

summarySTy :: Int -> STy -> String
summarySTy n (True,a) = summaryTy n a
summarySTy _ (False,TyDyn) = "void"
summarySTy n (False,a) = "void`" ++ summaryTy n a


summaryText :: Int -> Ty -> Maybe Text
summaryText maxLen = tyToVal >=> valToText >=> pure . trimText where
    trimText t = 
        if (T.compareLength t maxLen) /= GT then t else
        T.take (maxLen - 3) t `T.append` T.pack "..."

tyToVal :: Ty -> Maybe (V c)
tyToVal TyDyn = Nothing
tyToVal (TyNum (Just r)) = pure (N r)
tyToVal (TyNum Nothing) = Nothing
tyToVal (TyBlock kf (Just ops)) = pure $ B kf abc where
    abc = ABC { abc_code = ops
              , abc_comp = error "just typechecking" }
tyToVal (TyBlock _ Nothing) = Nothing
tyToVal (TyProd a b) = P <$> tyToVal a <*> tyToVal b
tyToVal (TySum a b) | (not (fst a)) = R <$> tyToVal (snd b)
                    | (not (fst b)) = L <$> tyToVal (snd a)
                    | otherwise = Nothing
tyToVal TyUnit = pure U
tyToVal (TySeal tok v) = S tok <$> tyToVal v

valToTy :: V c -> Ty
valToTy (L a) = TySum (True, valToTy a) (False, TyDyn)
valToTy (R b) = TySum (False, TyDyn) (True, valToTy b)
valToTy (N r) = TyNum (Just r)
valToTy (P a b) = TyProd (valToTy a) (valToTy b)
valToTy (B kf abc) = TyBlock kf (Just (abc_code abc))
valToTy U = TyUnit
valToTy (S tok v) = TySeal tok (valToTy v)
valToTy (TC _) = TyDyn

typeOfABC :: S.Seq Op -> Either Text (Ty, Ty)
typeOfABC = undefined runOp

newtype E = E { inE :: Text }
instance Error E where strMsg = E . T.pack
type TyM = ErrorT E Identity

runTyM :: TyM a -> Either Text a
runTyM = left inE . runIdentity . runErrorT

runOps :: S.Seq Op -> Ty -> TyM Ty
runOps = flip (S.foldlM (flip runOp))

runOp :: Op -> Ty -> TyM Ty
runOp (Op c) = runOpC c
runOp (TL txt) = return . TyProd ((valToTy . textToVal) txt)
runOp (BL ops) = return . TyProd (TyBlock kf0 (Just ops))
runOp (Invoke tok) = case T.uncons tok of
    Just ('&', _) -> return
    Just (':', sealer) -> return . TySeal sealer
    Just ('.', sealer) -> runOpUnseal sealer
    _ -> (const . return) TyDyn
runOp (AMBC _) = (const . fail) $ "ambiguity not supported"

runOpUnseal :: Text -> Ty -> TyM Ty
runOpUnseal s (TySeal tok ty) | (s == tok) = return ty
runOpUnseal _ (TyDyn) = return TyDyn
runOpUnseal tok v = fail $ "unsealer " ++ T.unpack tok ++ " @ " ++ show v

runOpC :: Char -> Ty -> TyM Ty
runOpC ' ' = pure
runOpC '\n' = pure
runOpC 'l' = runOp_l
runOpC 'r' = runOp_r
runOpC 'w' = runOp_w
runOpC 'z' = runOp_z
runOpC 'v' = runOp_v
runOpC 'c' = runOp_c
runOpC '^' = runOp_copy
runOpC '%' = runOp_drop
runOpC '$' = runOp_ap
runOpC 'o' = runOp_comp
runOpC '\'' = runOp_quote
runOpC 'k' = runOp_rel
runOpC 'f' = runOp_aff
runOpC '#' = return . TyProd (TyNum (Just 0))
runOpC c | (('0' <= c) && (c <= '9')) = 
    runOp_digit (fromEnum c - 48)
runOpC '+' = runOp_add
runOpC '-' = runOp_neg
runOpC '*' = runOp_mul
runOpC '/' = runOp_inv
runOpC 'Q' = runOp_div
runOpC 'L' = runOp_L
runOpC 'R' = runOp_R
runOpC 'W' = runOp_W
runOpC 'Z' = runOp_Z
runOpC 'V' = runOp_V
runOpC 'C' = runOp_C
runOpC '?' = runOp_condap
runOpC 'D' = runOp_distrib
runOpC 'F' = runOp_factor
runOpC 'M' = runOp_merge
runOpC 'K' = runOp_assert
runOpC 'P' = runOp_isProd
runOpC 'S' = runOp_isSum
runOpC 'B' = runOp_isBlock
runOpC 'N' = runOp_isNum
runOpC '>' = runOp_gt
runOpC  c  = \ v -> fail $ c : (" (unknown op) @ " ++ show v)

asProd :: Ty -> TyM (Ty,Ty)
asSum  :: Ty -> TyM (STy,STy)
asSum' :: STy -> TyM (STy, STy)
mkSum' :: STy -> STy -> STy
sumVoid, dynOpt :: STy
asNum  :: Ty -> TyM (Maybe Rational)
asBlock :: Ty -> TyM (KF, Maybe (S.Seq Op))
asUnit :: Ty -> TyM ()
isDyn, isProd, isSum, isBlock, isNum, isObs :: Ty -> Bool

asProd (TyProd a b) = return (a,b)
asProd TyDyn = return (TyDyn, TyDyn)
asProd v = fail $ "expected product @ " ++ show v

asSum (TySum a b) = return (a,b)
asSum TyDyn = return (dynOpt,dynOpt)
asSum v = fail $ "expected sum @ " ++ show v

sumVoid = (False,TyDyn)
dynOpt = (True,TyDyn)

asSum' (bLive, ab) =
    asSum ab >>= \ (a,b) ->
    let a' = (bLive && fst a, snd a) in
    let b' = (bLive && fst b, snd b) in
    return (a',b')

mkSum' a b = (fst a || fst b, TySum a b)

asNum (TyNum r) = return r
asNum TyDyn = return Nothing
asNum v = fail $ "expected number @ " ++ show v

asBlock (TyBlock kf ops) = return (kf,ops)
asBlock TyDyn = return (kf0, Nothing)
asBlock v = fail $ "expected block @ " ++ show v

asUnit (TyUnit) = return ()
asUnit (TyDyn) = return ()
asUnit v = fail $ "expected unit @ " ++ show v

isDyn TyDyn = True
isDyn _ = False

isProd (TyProd _ _) = True
isProd _ = False

isSum (TySum _ _) = True
isSum _ = False

isBlock (TyBlock _ _) = True
isBlock _ = False

isNum (TyNum _) = True
isNum _ = False

isObs (TySeal _ _) = False
isObs (TyUnit) = False
isObs _ = True

runOp_digit :: Int -> Ty -> TyM Ty
runOp_digit d vne =
    asProd vne >>= \ (vn,ve) ->
    asNum vn >>= \ mbr -> 
    let mbr' = updateDigit d <$> mbr in
    return $ TyProd (TyNum mbr') ve

updateDigit :: Int -> Rational -> Rational
updateDigit d r = 10*r + fromIntegral d

runOp_l, runOp_r, runOp_w, runOp_z, runOp_v, runOp_c,
 runOp_L, runOp_R, runOp_W, runOp_Z, runOp_V, runOp_C,
 runOp_copy, runOp_drop,
 runOp_ap, runOp_comp, runOp_quote, runOp_rel, runOp_aff,
 runOp_add, runOp_neg, runOp_mul, runOp_inv, runOp_div,
 runOp_condap, runOp_distrib, runOp_factor, runOp_merge, runOp_assert,
 runOp_isProd, runOp_isSum, runOp_isBlock, runOp_isNum, runOp_gt
    :: Ty -> TyM Ty

runOp_l abc =
    asProd abc >>= \ (a,bc) ->
    asProd bc >>= \ (b,c) ->
    return (TyProd (TyProd a b) c)
runOp_r abc =
    asProd abc >>= \ (ab,c) ->
    asProd ab >>= \ (a,b) ->
    return (TyProd a (TyProd b c))
runOp_w abc =
    asProd abc >>= \ (a,bc) ->
    asProd bc >>= \ (b,c) ->
    return (TyProd b (TyProd a c))
runOp_z abcd =
    asProd abcd >>= \ (a,bcd) ->
    asProd bcd >>= \ (b,cd) ->
    asProd cd >>= \ (c,d) ->
    return (TyProd a (TyProd c (TyProd b d)))
runOp_v x = return $ TyProd x TyUnit
runOp_c xu = asProd xu >>= \ (x, u) -> asUnit u >> return x

runOp_L abce =
    asProd abce >>= \ (abc,e) ->
    asSum  abc >>= \ (a,bc) ->
    asSum' bc  >>= \ (b,c) ->
    let sum' = TySum (mkSum' a b) c in
    return (TyProd sum' e)
runOp_R abce =
    asProd abce >>= \ (abc,e) ->
    asSum abc >>= \ (ab,c) ->
    asSum' ab >>= \ (a,b) ->
    let sum' = TySum a (mkSum' b c) in
    return (TyProd sum' e)
runOp_W abce =
    asProd abce >>= \ (abc,e) ->
    asSum abc >>= \ (a,bc) ->
    asSum' bc >>= \ (b,c) ->
    let sum' = TySum b (mkSum' a c) in
    return (TyProd sum' e)
runOp_Z abcde =
    asProd abcde >>= \ (abcd,e) ->
    asSum abcd >>= \ (a,bcd) ->
    asSum' bcd >>= \ (b,cd) ->
    asSum' cd >>= \ (c,d) ->
    let sum' = TySum a (mkSum' c (mkSum' b d)) in
    return (TyProd sum' e)
runOp_V ae =
    asProd ae >>= \ (a,e) ->
    let sum' = TySum (True,a) (False, TyDyn) in
    return (TyProd sum' e)
runOp_C ave =
    asProd ave >>= \ (av,e) ->
    if isDyn av then return (TyProd TyDyn e) else
    asSum av >>= \ (a,v) ->
    if fst v then fail $ "C @ " ++ show ave else
    return (TyProd (snd a) e)


runOp_copy ae =
    asProd ae >>= \ (a,e) ->
    if (not . tyCopyable) a 
        then fail $ "^ @ " ++ show ae 
        else return (TyProd a (TyProd a e))

tyCopyable :: Ty -> Bool
tyCopyable TyDyn = True
tyCopyable (TyNum _) = True
tyCopyable (TyBlock kf _) = may_copy kf
tyCopyable (TyProd a b) = tyCopyable a && tyCopyable b
tyCopyable (TyUnit) = True
tyCopyable (TySum a b) = tyCopyable (snd a) && tyCopyable (snd b)
tyCopyable (TySeal _ v) = tyCopyable v

runOp_drop ae =
    asProd ae >>= \ (a,e) ->
    if (not . tyDroppable) a 
        then fail $ "% @ " ++ show ae 
        else return e

tyDroppable :: Ty -> Bool
tyDroppable TyDyn = True
tyDroppable (TyNum _) = True
tyDroppable (TyBlock kf _) = may_drop kf
tyDroppable (TyProd a b) = tyDroppable a && tyDroppable b
tyDroppable TyUnit = True
tyDroppable (TySum a b) = tyDroppable (snd a) && tyDroppable (snd b)
tyDroppable (TySeal _ v) = tyDroppable v

runOp_ap bxe =
    asProd bxe >>= \ (b,xe) ->
    asProd xe >>= \ (x,e) ->
    asBlock b >>= \ (_,mbops) ->
    case mbops of
        Nothing -> return (TyProd TyDyn e)
        Just ops -> case runTyM (runOps ops x) of
            Right x' -> return (TyProd x' e)
            Left err ->
                let emsg1 = "failure in '$' call @ " ++ show bxe in 
                let emsg2 = indent "  " (T.unpack err) in
                fail $ emsg1 ++ "\n" ++ emsg2

indent :: String -> String -> String
indent ws = L.unlines . L.map (ws ++) . L.lines 

runOp_comp yzxye =
    asProd yzxye >>= \ (yz,xye) ->
    asProd xye >>= \ (xy,e) ->
    asBlock yz >>= \ (kfyz,mbopsyz) ->
    asBlock xy >>= \ (kfxy,mbopsxy) ->
    let kfxz = kfxy `mappend` kfyz in
    let mbopsxz = (S.><) <$> mbopsxy <*> mbopsyz in
    let xz = TyBlock kfxz mbopsxz in
    return $ TyProd xz e

runOp_quote ve =
    asProd ve >>= \(v,e) ->
    return $ TyProd (tyQuote v) e

tyQuote :: Ty -> Ty
tyQuote (TyNum (Just r)) = TyBlock kf0 (Just (abcQuote (N r)))
tyQuote (TyBlock kf (Just ops)) = TyBlock kf (Just ops') where
    ops' = (addf . addk) $ S.singleton (BL ops) 
    addk = if (may_drop kf) then id else (S.|> Op 'k')
    addf = if (may_copy kf) then id else (S.|> Op 'f')
tyQuote v = TyBlock kf Nothing where
    kf = KF { may_copy = tyCopyable v
            , may_drop = tyDroppable v }

runOp_rel be =
    asProd be >>= \ (b,e) ->
    asBlock b >>= \ (kf,ops) ->
    let kf' = kf { may_drop = False } in
    let b' = TyBlock kf' ops in
    return $ TyProd b' e

runOp_aff be =
    asProd be >>= \ (b,e) ->
    asBlock b >>= \ (kf,ops) ->
    let kf' = kf { may_copy = False } in
    let b' = TyBlock kf' ops in
    return $ TyProd b' e

runOp_add abe =
    asProd abe >>= \ (a,be) ->
    asProd be >>= \ (b,e) ->
    asNum a >>= \ na ->
    asNum b >>= \ nb ->
    let n' = TyNum $ (+) <$> na <*> nb in
    return $ TyProd n' e

runOp_neg ae =
    asProd ae >>= \ (a,e) ->
    asNum a >>= \ na ->
    let n' = TyNum $ negate <$> na in
    return $ TyProd n' e

runOp_mul abe =
    asProd abe >>= \ (a,be) ->
    asProd be >>= \ (b,e) ->
    asNum a >>= \ na ->
    asNum b >>= \ nb ->
    let n' = TyNum $ (*) <$> na <*> nb in
    return $ TyProd n' e

runOp_inv ae =
    asProd ae >>= \ (a,e) ->
    asNum a >>= \ na ->
    let divByZero = (Just 0 == na) in
    if divByZero then fail $ "divByZero (op /) @ " ++ show ae else
    let n' = TyNum $ recip <$> na in
    return $ TyProd n' e

runOp_div abe =
    asProd abe >>= \ (a,be) ->
    asProd be >>= \ (b,e) ->
    asNum a >>= \ na ->
    asNum b >>= \ nb ->
    let divByZero = (Just 0 == na) in
    if divByZero then fail $ "divByZero (op Q) @ " ++ show abe else
    let nrq = divModQ <$> na <*> nb in
    let nr = TyNum $ fst <$> nrq in
    let nq = TyNum $ (fromIntegral . snd) <$> nrq in
    return $ TyProd nr (TyProd nq e)

runOp_condap bse =
    asProd bse >>= \ (b,se) ->
    asProd se >>= \ (s,e) ->
    asSum s >>= \ (l,r) ->
    asBlock b >>= \ (kf,mbops) ->
    let eUntyDroppable = "? (w/untyDroppable) @ " ++ show bse in
    if not (may_drop kf) then fail eUntyDroppable else
    case mbops of
        Nothing ->
            let s' = TySum (fst l, TyDyn) r in
            return $ TyProd s' e
        Just ops -> case runTyM (runOps ops (snd l)) of
            Right l' ->
                let s' = TySum (fst l, l') r in
                return $ TyProd s' e
            Left etxt ->
                let emsg1 = "failure in '?' call @ " ++ show bse in
                let emsg2 = indent " " $ T.unpack etxt in
                fail $ emsg1 ++ "\n" ++ emsg2

runOp_distrib abce =
    asProd abce >>= \ (a, bce) -> 
    asProd bce >>= \ (bc,e) ->
    asSum bc >>= \ (b,c) ->
    let b' = (fst b, TyProd a (snd b)) in
    let c' = (fst c, TyProd a (snd c)) in
    return $ TyProd (TySum b' c') e

runOp_factor se =
    asProd se >>= \ (s,e) ->
    asSum s >>= \ (ab,cd) ->
    asProd (snd ab) >>= \ (a,b) ->
    asProd (snd cd) >>= \ (c,d) ->
    let ac = TySum (fst ab, a) (fst cd, c) in
    let bd = TySum (fst ab, b) (fst cd, d) in
    return $ TyProd ac (TyProd bd e)

runOp_merge se =
    asProd se >>= \ (s,e) ->
    asSum s >>= \ (a,b) ->
    return $ TyProd (snd $ tyMerge' a b) e

tyMerge' :: STy -> STy -> STy
tyMerge' (False,_) (True,b) = (True,b)
tyMerge' (True,a) (False,_) = (True,a)
tyMerge' a b = (fst a || fst b, tyMerge (snd a) (snd b))

-- permissive merge, because I cannot locally determine 
-- future compatibility. 
--
-- A better alternative might be to represent the merged
-- type directly (e.g. `TyMerge a b`). But I don't want to
-- deal with that quite yet.
tyMerge :: Ty -> Ty -> Ty
tyMerge TyDyn _ = TyDyn
tyMerge _ TyDyn = TyDyn
tyMerge TyUnit TyUnit = TyUnit
tyMerge (TyProd a b) (TyProd c d) = TyProd (tyMerge a c) (tyMerge b d)
tyMerge (TySum a b) (TySum c d) = TySum (tyMerge' a c) (tyMerge' b d)
tyMerge (TySeal s1 v1) (TySeal s2 v2) | s1 == s2 = TySeal s1 (tyMerge v1 v2)
tyMerge (TyNum r1) (TyNum r2) = if (r1 == r2) then TyNum r1 else TyNum Nothing
tyMerge (TyBlock kf1 ops1) (TyBlock kf2 ops2) =
    let kf' = kf1 `mappend` kf2 in
    if (ops1 == ops2) then TyBlock kf' ops1
        else TyBlock kf' Nothing
tyMerge _ _ = TyDyn


runOp_assert se =
    asProd se >>= \ (s,e) ->
    asSum s >>= \ (a,b) ->
    let staticAssertFailure = fst a && not (fst b) in
    let msgFail = "static assertion failure 'K' @ " ++ show se in
    if staticAssertFailure then fail msgFail else
    return $ TyProd (snd b) e



asObsProd :: Ty -> TyM (Ty, Ty)
asObsProd oe =
    asProd oe >>= \ (o,e) ->
    if isObs o then return (o,e) else
    fail $ "expecting observable @ " ++ show oe

runOp_isProd ve =
    asObsProd ve >>= \ (v,e) ->
    let dynProd = TyProd TyDyn TyDyn in
    let vo = if isDyn v then TySum dynOpt (True,dynProd) else
             if isProd v then TySum sumVoid (True,v) else
             TySum (True,v) (False,dynProd)
    in
    return $ TyProd vo e

runOp_isSum ve = 
    asObsProd ve >>= \ (v,e) ->
    let dynSum = TySum dynOpt dynOpt in
    let vo = if isDyn v then TySum dynOpt (True,dynSum) else
             if isSum v then TySum sumVoid (True,v) else
             TySum (True,v) (False,dynSum)
    in
    return $ TyProd vo e

runOp_isNum ve =
    asObsProd ve >>= \ (v,e) ->
    let dynNum = TyNum Nothing in
    let vo = if isDyn v then TySum dynOpt (True, dynNum) else
             if isNum v then TySum sumVoid (True, v) else
             TySum (True, v) (False, dynNum)
    in
    return $ TyProd vo e

runOp_isBlock ve =
    asObsProd ve >>= \ (v,e) ->
    let dynBlock = TyBlock kf0 Nothing in
    let vo = if isDyn v then TySum dynOpt (True, dynBlock) else
             if isBlock v then TySum sumVoid (True, v) else
             TySum (True, v) (False, dynBlock)
    in
    return $ TyProd vo e


runOp_gt = undefined

