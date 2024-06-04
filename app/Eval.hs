module Eval where
import           Ast         (Op (..), Pattern (..), Term (..), Type (..))
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Set    (Set)
import           Debug.Trace (trace)

type Context = Map String Term


initialCtx :: Context
initialCtx = Map.fromList [("a", TmInt 5), ("x", BoolLit False), ("b", TmInt 4)]

lookfor :: Context -> String -> Term
lookfor ctx s = case Map.lookup s ctx of
    Just val -> val
    Nothing  -> Var ("Nothing")

eval :: Context -> Term -> Term
eval ctx (Var x)                 = lookfor ctx x
eval ctx (TmApp f a)             =
  case eval ctx f of
    TmAbs x _ b -> eval (Map.insert x (eval ctx a) ctx) b
    f'          -> TmApp f' (eval ctx a)
eval ctx (TmAbs x t b)           = TmAbs x t (eval ctx b)
eval ctx (BinOp op a b)          = case (op, a, b) of
    (Add, TmInt(x), TmInt(y))-> TmInt(x + y)
    (Add, Var(a), Var(b)) -> do
       let a_                    = lookfor ctx a
       let b_                    = lookfor ctx b
       case (a_, b_) of
           (TmInt(x), TmInt(y)) -> TmInt(x + y)
eval ctx (Let x t e b)           =
  let e'                         = eval ctx e
      ctx'                       = Map.insert x e' ctx
  in eval ctx' b
eval _ t@TmTrue                  = t
eval _ t@TmFalse                 = t
eval _ t@(TmInt _)               = t
eval _ t@TyBool                  = t
eval _ t@TyInt                   = t
eval _ t@(BoolLit _)             = t
eval _ t@(Kind _)                = t
eval ctx (Pi x k t)              = Pi x (eval ctx k) (eval ctx t)

eval ctx (TmMatch expr branches) =
  trace ("Evaluating MATCH with expr: " ++ show expr ++ " " ++ show branches) $
  case eval ctx expr of
      Var v -> eval ctx (Var v)
      val@(BoolLit b) -> eval ctx $ match val branches
      val@(TmInt i)   -> eval ctx $ match val branches
      result -> trace ("Pattern match failure: " ++ show result ++ " ---- " ++ show expr) $
          error $ "Pattern match failure: " ++ show expr
    where
        match _ []               = error "Pattern match failure: no matching pattern found"
        match val ((p, body):bs) = trace ("matching " ++ show val ++ " against: " ++ show p ++ " " ++ show body ++ " " ++ show bs) $
            if patternMatches val p
            then body
            else match val bs

patternMatches :: Term -> Pattern -> Bool
patternMatches (BoolLit b) (PBool pb) = b == pb
patternMatches (TmInt i) (PInt pi)    = trace("Are both value and pattern equal? ") $ i == pi
patternMatches x Wildcard             = trace("Do not care about the pattern. Return the body of this branch. " ++ show x ++ " match with " ++ "_ " ++ "-> <body>") $ True
patternMatches _ _                    = trace ("Any other case. ") $ False

prune :: Term -> Type -> (Term, Type)
prune (TmAbs _ _ e) (Pi _ _ t) = prune e t
prune e t                      = (e, t)
