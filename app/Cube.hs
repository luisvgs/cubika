module Cube where
import           Ast
import           Control.Monad                  ( when )
import           Control.Monad.State
import           Data.Either                    ( fromRight )
import           Data.List                      ( elemIndex )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Debug.Trace                    ( trace )

newtype TypeError = TypeError String
  deriving Show

freeVars :: Term -> Set String
freeVars (Var s)       = Set.singleton s
freeVars TmTrue        = Set.empty
freeVars TmFalse       = Set.empty
freeVars (TmInt _    ) = Set.empty
freeVars (TmApp f a  ) = freeVars f `Set.union` freeVars a
freeVars (TmAbs i t e) = freeVars t `Set.union` freeVars e
freeVars (Pi    i k t) = freeVars k `Set.union` freeVars t
freeVars (Kind _     ) = Set.empty
-- freeVars _             = Set.empty

-- Predefined terms
-- Pi \ a : * \ x : a . x
tmId :: Term
-- tmId = TmAbs "a" (Kind Star) $ TmAbs "x" (TmVar "a") (TmVar "x")
-- \x : Type . x : Type
tmId = TmAbs "x" (Kind Star) $ Var "x"

-- \f : Nat -> Nat . \z : Nat. z
zero :: Term
zero =
  TmAbs "f" (TmAbs "a" (Kind Star) $ Var "a") $ TmAbs "z" (Var "Nat") $ Var "z"

-- \ f : Nat . \z . f z
-- succ :: Term
-- succ = TmAbs "f" (TmVar "Nat") $ TmAbs "x" ()


--Πx:*. B x
dft :: Term
dft = Pi "x" (Var "A") (TmApp (Var "B") (Var "x"))

--Πa:*.Πx:a.a
id' = TmAbs "a" (Kind Star) $ TmAbs "x" (Var "a") (Var "x")
idt = Pi "a" (Kind Star) $ Pi "x" (Var "a") (Var "a")

type Context = Map String Type

extendContext :: String -> Type -> Context -> Context
extendContext v t c = Map.insert v t c

-- typeOfStmt :: Context -> Stmt -> Either TypeError (Stmt, Type)

typeOf :: Context -> Term -> Either TypeError (Term, Type)
typeOf ctx b@TmTrue         = Right (b, TyBool)
typeOf ctx b@TmFalse        = Right (b, TyBool)
typeOf ctx b@(BoolLit b'  ) = Right (b, TyBool)
typeOf ctx i@(TmInt   n   ) = Right (i, TyInt)
typeOf ctx a@(Assign v t e) = do
  -- TODO: check if t has form of A, B, U, etc
  u <- typeOf ctx t
  let (_, u') = u
  e_ <- typeOf ctx e
  let (_, t') = e_
  let _       = extendContext v t ctx
  -- when (t /= t') $ Left $ TypeError $ "Types are not equal: " ++ show (t, t')
  return (a, u')
typeOf ctx x@(Var v) = case Map.lookup v ctx of
  Just ty -> Right (x, ty)
  Nothing -> Left $ TypeError v
typeOf ctx e@(TmAbs x ty t) = do
  _ <- typeOf ctx ty
  let ctx' = Map.insert x ty ctx
  ty_ <- typeOf ctx' t
  let (_, ty') = ty_
  let lt       = Pi x ty ty'
  _ <- typeOf ctx lt
  return (e, lt)
typeOf ctx e@(TmApp t1 t2) = do
  -- Typechek t1 in the context
  ty1' <- typeOf ctx t1
  let (_, ty1) = ty1'
  case ty1 of
    -- (Π a:x . B) y
    -- at: argument type
    -- rt: return type
    Pi x at rt -> do
      -- Typechek t2 in the context
      ta' <- typeOf ctx t2
      let (_, ta) = ta'
      -- (betaEq x y)?
      -- "Since types can now be arbitrary expression
      -- we use betaEq to compare them instead of (==).""
      unless (betaEq ta at) $ Left $ TypeError "Bad function arguments"
      Right $ (e, subst x t2 rt)
    _ -> Left $ TypeError "Non-function in application"
typeOf _   k@( Kind Star) = return $ (k, Kind Box)
typeOf _   (   Kind Box ) = Left $ TypeError "Found box"
typeOf ctx pi@(Pi x a b ) = do
  s <- tCheckRed ctx a
  let r' = Map.insert x a ctx
  t <- tCheckRed r' b
  trace ("current" ++ show (s, t)) (return ())
  when ((s, t) `notElem` allowedKinds)
    $  Left
    $  TypeError
    $  "Bad abstraction"
    ++ show (s, t)
  return (pi, t)

allowedKinds :: [(Type, Type)]
allowedKinds =
  [ (Kind Star, Kind Star)
  , (Kind Star, Kind Box)
  , (Kind Box , Kind Star)
  , (Kind Box , Kind Box)
  ]

-- Type checks and normalizes the result.
tCheckRed r e = do
  (_, t) <- typeOf r e
  return (whnf e)

whnf :: Term -> Term
whnf ee = spine ee []
 where
  spine (TmApp f a  ) as       = spine f (a : as)
  spine (TmAbs s _ e) (a : as) = spine (subst s a e) as
  spine f             as       = foldl TmApp f as

subst :: String -> Term -> Term -> Term
subst v x = sub
 where
  sub e@(Var i      ) = if i == v then x else e
  sub (  TmApp f a  ) = TmApp (sub f) (sub a)
  sub (  TmAbs i t e) = abstr TmAbs i t e
  sub (  Pi    i t e) = abstr Pi i t e
  sub (  Kind k     ) = Kind k
  fvx = Set.toList $ freeVars x
  cloneSym e i = loop i
   where
    loop i' = if i' `elem` vars then loop (i ++ "'") else i'
    vars = fvx ++ Set.toList (freeVars e)
  abstr con i t e = if v == i
    then con i (sub t) e
    else if i `elem` fvx
      then
        let i' = cloneSym e i
            e' = substVar i i' e
        in  con i' (sub t) (sub e')
      else con i (sub t) (sub e)

betaEq :: Term -> Term -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

nf :: Term -> Term
nf ee = spine ee []
 where
  spine (TmApp f a  ) as       = spine f (a : as)
  spine (TmAbs s t e) []       = TmAbs s (nf t) (nf e)
  spine (TmAbs s _ e) (a : as) = spine (subst s a e) as
  spine (Pi    s k t) as       = app (Pi s (nf k) (nf t)) as
  spine f             as       = app f as
  app f as = foldl TmApp f (map nf as)

alphaEq :: Term -> Term -> Bool
alphaEq (Var v      ) (Var v'       ) = v == v'
alphaEq (TmApp f a  ) (TmApp f' a'  ) = alphaEq f f' && alphaEq a a'
alphaEq (TmAbs s _ e) (TmAbs s' _ e') = alphaEq e (substVar s' s e')
alphaEq _             _               = False

substVar :: String -> String -> Term -> Term
substVar s s' = subst s (Var s')

pretty :: Type -> String
pretty TmFalse      = "Bool"
pretty TmTrue       = "Bool"
pretty TyBool       = "Bool"
pretty (TmInt _)    = "Int"
pretty TyInt        = "Int"
pretty (Pi s t1 t2) = "Pi " ++ s ++ ":" ++ pretty t1 ++ " . " ++ pretty t2
pretty (Kind Star ) = "*"
pretty (Kind Box  ) = "⬛"
