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
    , tyRunOp, tyRevOp
    , typeOfABC 
    ) where

import Data.Ratio
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
    | TyMerge Ty Ty -- a merged choice of values
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
    case summaryText 24 v of
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
summaryTy n (TyMerge a b) = "(" ++ summaryTy (n-1) a ++ "|" ++ summaryTy (n-1) b ++ ")"

summarySTy :: Int -> STy -> String
summarySTy n (True,a) = summaryTy n a
summarySTy _ (False,TyDyn) = "void"
summarySTy n (False,a) = "void`" ++ summaryTy n a


summaryText :: Int -> Ty -> Maybe Text
summaryText maxLen = tyToVal >=> valToText >=> pure . trimText where
    trimText t = 
        if (T.compareLength t maxLen) /= GT 
            then t 
            else T.pack "text"

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
tyToVal (TyMerge _ _) = Nothing

valToTy :: V c -> Ty
valToTy (L a) = TySum (True, valToTy a) (False, TyDyn)
valToTy (R b) = TySum (False, TyDyn) (True, valToTy b)
valToTy (N r) = TyNum (Just r)
valToTy (P a b) = TyProd (valToTy a) (valToTy b)
valToTy (B kf abc) = TyBlock kf (Just (abc_code abc))
valToTy U = TyUnit
valToTy (S tok v) = TySeal tok (valToTy v)
valToTy (TC _) = TyDyn 

-- | extract the (input,output) types from a segment of code. 
-- This will start without any assumptions about type, then 
-- refine type information in multiple passes. 
typeOfABC :: S.Seq Op -> Either Text (Ty, Ty)
typeOfABC ops = runTyM $
    tyPassAnno TyDyn ops >>= \ (annoOps1, tyOut1) ->
    tyPassRev tyOut1 annoOps1 >>= \ tyIn1 ->
    tyPassAnno tyIn1 ops >>= \ (annoOps2, tyOut2) ->
    tyPassRev tyOut2 annoOps2 >>= \ tyIn2 ->
    return (tyIn2, tyOut2) -- 2 passes is enough

tyPassAnno :: Ty -> S.Seq Op -> TyM (S.Seq (Ty,Op), Ty)
tyPassAnno = step S.empty where
    step sR v ops = case S.viewl ops of
        S.EmptyL -> return (sR, v)
        (op S.:< ops') ->
            let sR' = sR S.|> (v,op) in
            runOp op v >>= \ v' ->
            step sR' v' ops'

tyPassRev :: Ty -> S.Seq (Ty,Op) -> TyM Ty
tyPassRev _ _ = return TyDyn


newtype E = E { inE :: Text }
instance Error E where strMsg = E . T.pack
type TyM = ErrorT E Identity

runTyM :: TyM a -> Either Text a
runTyM = left inE . runIdentity . runErrorT

runOps :: S.Seq Op -> Ty -> TyM Ty
runOps = flip (S.foldlM (flip runOp))

runOp, tyRunOp :: Op -> Ty -> TyM Ty
runOp (Op c) = runOpC c
runOp (TL txt) = return . TyProd ((valToTy . textToVal) txt)
runOp (BL ops) = return . TyProd (TyBlock kf0 (Just ops))
runOp (Invoke tok) = case T.uncons tok of
    Just ('&', _) -> return
    Just (':', sealer) -> return . TySeal sealer
    Just ('.', sealer) -> runOpUnseal sealer
    _ -> (const . return) TyDyn
runOp (AMBC _) = (const . fail) $ "ambiguity not supported"

tyRunOp = runOp

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
isDyn, isProd, isSum, isBlock, isNum, isObs, isSealed :: Ty -> Bool

asProd (TyProd a b) = return (a,b)
asProd TyDyn = return (TyDyn, TyDyn)
asProd (TyMerge l r) =
    asProd l >>= \ (l1,l2) ->
    asProd r >>= \ (r1,r2) ->
    let m1 = tyMerge l1 r1 in
    let m2 = tyMerge l2 r2 in
    return (m1,m2)
asProd v = fail $ "expected product @ " ++ show v

asSum (TySum a b) = return (a,b)
asSum TyDyn = return (dynOpt,dynOpt)
asSum (TyMerge l r) =
    asSum l >>= \ (l1, l2) ->
    asSum r >>= \ (r1, r2) ->
    let m1 = (fst l1 || fst r1, tyMerge (snd l1) (snd r1)) in
    let m2 = (fst l2 || fst r2, tyMerge (snd l2) (snd r2)) in
    return (m1,m2)
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
asNum (TyMerge a b) =
    asNum a >>= \ na ->
    asNum b >>= \ nb ->
    if (na == nb) then return na else 
    return Nothing
asNum v = fail $ "expected number @ " ++ show v

asBlock (TyBlock kf ops) = return (kf,ops)
asBlock TyDyn = return (kf0, Nothing)
asBlock (TyMerge a b) =
    asBlock a >>= \ (kfa,ba) ->
    asBlock b >>= \ (kfb,bb) ->
    let kf' = kfa `mappend` kfb in
    if (ba == bb) then return (kf',ba) else 
    return (kf',Nothing)
asBlock v = fail $ "expected block @ " ++ show v

asUnit (TyUnit) = return ()
asUnit (TyDyn) = return ()
asUnit (TyMerge a b) = asUnit a >> asUnit b
asUnit v = fail $ "expected unit @ " ++ show v

isDyn TyDyn = True
isDyn (TyMerge a b) = isDyn a && isDyn b
isDyn _ = False

isProd (TyProd _ _) = True
isProd (TyMerge a b) = isProd a && isProd b
isProd _ = False

isSum (TySum _ _) = True
isSum (TyMerge a b) = isSum a && isSum b
isSum _ = False

isBlock (TyBlock _ _) = True
isBlock (TyMerge a b) = isBlock a && isBlock b
isBlock _ = False

isNum (TyNum _) = True
isNum (TyMerge a b) = isNum a && isNum b
isNum _ = False

isObs (TySeal _ _) = False
isObs (TyUnit) = False
isObs (TyMerge a b) = isObs a && isObs b
isObs _ = True

isSealed (TySeal _ _) = True
isSealed (TyMerge a b) = isSealed a && isSealed b
isSealed _ = False

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
tyCopyable (TyMerge a b) = tyCopyable a && tyCopyable b

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
tyDroppable (TyMerge a b) = tyDroppable a && tyDroppable b

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
tyMerge TyDyn b = b -- future must be at least `b`-compatible
tyMerge a TyDyn = a -- future must be at least `a`-compatible
tyMerge TyUnit TyUnit = TyUnit
tyMerge (TyProd a b) (TyProd c d) = TyProd (tyMerge a c) (tyMerge b d)
tyMerge (TySum a b) (TySum c d) = TySum (tyMerge' a c) (tyMerge' b d)
tyMerge (TySeal s1 v1) (TySeal s2 v2) | s1 == s2 = TySeal s1 (tyMerge v1 v2)
tyMerge (TyNum r1) (TyNum r2) | (r1 == r2) = TyNum r1
tyMerge (TyBlock kf1 ops1) (TyBlock kf2 ops2) | (ops1 == ops2) =
    let kf' = kf1 `mappend` kf2 in TyBlock kf' ops1
tyMerge a b = TyMerge a b

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

runOp_gt abe =  -- test if b > a in (a*(b*e))
    asProd abe >>= \ (a,be) ->
    asProd be >>= \ (b,e) ->
    case runTyM (tyGT b a) of
        Left etxt -> 
            let emsg1 = "invalid '>' comparison @ " ++ show abe in
            let emsg2 = indent "  " (T.unpack etxt) in
            fail $ emsg1 ++ "\n" ++ emsg2
        Right mbgt -> 
            let inL = TyProd b a in -- a >= b
            let inR = TyProd a b in -- b > a
            let vgt = case mbgt of
                    Nothing -> TySum (True, inL) (True, inR)
                    Just True -> TySum (False, inL) (True, inR)
                    Just False -> TySum (True, inL) (False, inR)
            in
            return $ TyProd vgt e

tyGT :: Ty -> Ty -> TyM (Maybe Bool)
tyGT TyDyn TyDyn = return Nothing
tyGT TyUnit TyUnit = return (Just False)
tyGT TyUnit v = fail $ "compare unit (inL) with " ++ show v
tyGT v TyUnit = fail $ "compare unit (inR) with " ++ show v
tyGT (TyNum x) (TyNum y) = return $ (>) <$> x <*> y
tyGT (TyProd x1 x2) (TyProd y1 y2) =
    tyGT x1 y1 >>= \ mbGT ->
    case mbGT of
        Nothing -> return Nothing
        Just True -> return (Just True)
        Just False ->
            tyGT y1 x1 >>= \ mbLT ->
            case mbLT of
                Nothing -> return Nothing
                Just True -> return (Just False)
                Just False -> tyGT x2 y2
tyGT (TySum x1 x2) (TySum y1 y2) = 
    let xInR = fst x2 && (not . fst) x1 in
    let xInL = fst x1 && (not . fst) x2 in
    let yInR = fst y2 && (not . fst) y1 in
    let yInL = fst y1 && (not . fst) y2 in
    if (xInL && yInL) then tyGT (snd x1) (snd y1) else
    if (xInR && yInL) then return (Just True) else
    if (xInR && yInR) then tyGT (snd x2) (snd y2) else
    if (xInL && yInR) then return (Just False) else
    return Nothing
tyGT x y | (isBlock x || isBlock y) = fail "cannot compare blocks"
tyGT x y | (isSealed x || isSealed y) = fail "cannot compare sealed values"
tyGT p ns | isProd p && (isNum ns || isSum ns) = return (Just True)
tyGT ns p | isProd p && (isNum ns || isSum ns) = return (Just False)
tyGT n s | isNum n && isSum s = return (Just True)
tyGT s n | isNum n && isSum s = return (Just False)
tyGT _ _ = return Nothing -- compare at runtime


-- revOp is 'op -> expected input -> output -> TyM refined input'
--  the expected output is mostly to leverage partial evaluations
revOp, tyRevOp :: Op -> Ty -> Ty -> TyM Ty
revOp (Op '$') = revOp_apply
revOp (Op '?') = revOp_condap
revOp (Op c) = const $ revOpC c
revOp (TL txt) = const $ revOp_textLit txt
revOp (BL ops) = const $ revOp_blockLit ops
revOp (Invoke tok) = case T.uncons tok of
    Just ('&',_) -> const return
    Just (':', sealer) -> const $ runOpUnseal sealer
    Just ('.', sealer) -> const $ return . TySeal sealer
    _ -> (const . const . return) TyDyn
revOp (AMBC _) = (const . const . fail) $ "ambiguity not supported"

tyRevOp = revOp

revOpC :: Char -> Ty -> TyM Ty
revOpC ' '  = pure
revOpC '\n' = pure
revOpC 'l'  = runOp_r
revOpC 'r'  = runOp_l
revOpC 'w'  = runOp_w
revOpC 'z'  = runOp_z
revOpC 'v'  = runOp_c
revOpC 'c'  = runOp_v
revOpC '^'  = revOp_copy
revOpC '%'  = revOp_drop
-- revOpC '$'  = revOp_apply
revOpC 'o'  = revOp_comp
revOpC '\'' = revOp_quote
revOpC 'k'  = revOp_rel
revOpC 'f'  = revOp_aff
revOpC '#'  = revOp_introNum 
revOpC c | (('0' <= c) && (c <= '9')) = revOp_digit (fromEnum c - 48)
revOpC '+'  = revOp_add
revOpC '-'  = revOp_neg
revOpC '*'  = revOp_mul
revOpC '/'  = revOp_inv
revOpC 'Q'  = revOp_div
revOpC 'L'  = runOp_R
revOpC 'R'  = runOp_L
revOpC 'W'  = runOp_W
revOpC 'Z'  = runOp_Z
revOpC 'V'  = runOp_C
revOpC 'C'  = runOp_V
-- revOpC '?'  = revOp_condap
revOpC 'D'  = revOp_distrib
revOpC 'F'  = revOp_factor
revOpC 'M'  = revOp_merge
revOpC 'K'  = revOp_assert
revOpC 'P'  = revOp_isProd
revOpC 'S'  = revOp_isSum
revOpC 'B'  = revOp_isBlock
revOpC 'N'  = revOp_isNum
revOpC '>'  = revOp_gt
revOpC  c   = \ v -> fail $ (c : " (unknown op) rev@ " ++ show v)

revOp_copy, revOp_drop, 
 revOp_comp, revOp_quote, revOp_rel, revOp_aff,
 revOp_add, revOp_neg, revOp_mul, revOp_inv, revOp_div,
 revOp_distrib, revOp_factor, revOp_merge, revOp_assert,
 revOp_isProd, revOp_isSum, revOp_isBlock, revOp_isNum, revOp_gt
    :: Ty -> TyM Ty

revOp_textLit  :: Text -> Ty -> TyM Ty
revOp_blockLit :: S.Seq Op -> Ty -> TyM Ty
revOp_introNum :: Ty -> TyM Ty
revOp_digit    :: Int -> Ty -> TyM Ty
revOp_apply    :: Ty -> Ty -> TyM Ty
revOp_condap   :: Ty -> Ty -> TyM Ty

revOp_textLit _ tv = asProd tv >>= \ (t,v) -> asText t >> return v
revOp_blockLit _ bv = asProd bv >>= \ (b,v) -> asBlock b >> return v
revOp_introNum nv = asProd nv >>= \ (n,v) -> asNum n >> return v
revOp_digit _ nv = 
    asProd nv >>= \ (n,v) -> 
    asNum n >>= \ n' -> 
    return (TyProd (TyNum n') v)

asText, asChar :: Ty -> TyM Ty
asText TyDyn = return TyDyn
asText (TyProd c t) = 
    asChar c >>= \ c' ->
    asText t >>= \ t' ->
    return (TyProd c' t')
asText v@(TyNum (Just r)) = 
    if (r == 3) then return v else
    fail $ "expecting text, but terminates in " ++ show r
asText v@(TyNum Nothing) = return v
asText v = fail $ "expecting text @ " ++ show v

asChar TyDyn = return (TyNum Nothing)
asChar v@(TyNum Nothing) = return v
asChar v@(TyNum (Just r)) =
    if isChar r then return v else 
    fail $ "text element is not a character"
asChar v = fail $ "expecting character @ " ++ show v

isChar :: Rational -> Bool
isChar r = isInteger && inRange where
    isInteger = (1 == denominator r)
    n = numerator r
    inRange = (0 <= n) && (n <= 0x10ffff)

-- currently revOp_apply is less precise than it could be
revOp_apply inBXE outXE = 
    asProd inBXE >>= \ (inB,inXE) ->
    asProd inXE >>= \ (inX, inEnv) ->
    asProd outXE >>= \ (_outX, outEnv) ->
    asBlock inB >>= \ (kf, mbops) ->
    let inEr = tyMerge inEnv outEnv in
    let inXrev = TyDyn in -- TODO: compute using outX & mbops
    let inXr = tyMerge inXrev inX in
    return (TyProd (TyBlock kf mbops) (TyProd inXr inEr))

-- currently revOp_condap is less precise than it could be
revOp_condap inBLRE outLRE = 
    asProd inBLRE >>= \ (inB, inLRE) ->
    asProd inLRE >>= \ (inLR, inEnv) ->
    asBlock inB >>= \ (kf, mbops) -> 
    asSum inLR >>= \ (inL, inR) ->
    asProd outLRE >>= \ (outLR, outEnv) ->
    asSum outLR >>= \ (outL, outR) ->
    let inRr = tyMerge' outR inR in
    let inEr = tyMerge inEnv outEnv in
    let inLrev = (fst outL, TyDyn) in -- TODO: compute using (snd outL) & ops
    let inLr = tyMerge' inLrev inL in
    let inLRr = TySum inLr inRr in
    return (TyProd (TyBlock kf mbops) (TyProd inLRr inEr))

revOp_copy = undefined
revOp_drop = undefined
revOp_comp = undefined
revOp_quote = undefined
revOp_rel = undefined
revOp_aff = undefined
revOp_add = undefined
revOp_neg = undefined
revOp_mul = undefined
revOp_inv = undefined
revOp_div = undefined
revOp_distrib = undefined
revOp_factor = undefined
revOp_merge = undefined
revOp_assert = undefined
revOp_isProd = undefined
revOp_isSum = undefined
revOp_isBlock = undefined
revOp_isNum = undefined 
revOp_gt = undefined
 
