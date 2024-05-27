module Cube where

import           Ast
import           Control.Monad       (when)
import           Control.Monad.State
import           Data.Either         (fromRight)
import           Data.List           (elemIndex)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromJust)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)

newtype TypeError = TypeError String
    deriving (Show)

freeVars :: Term -> Set String
freeVars (Var s)       = Set.singleton s
freeVars TmTrue        = Set.empty
freeVars TmFalse       = Set.empty
freeVars (TmInt _)     = Set.empty
freeVars (TmApp f a)   = freeVars f `Set.union` freeVars a
freeVars (TmAbs i t e) = freeVars t `Set.union` freeVars e
freeVars (Let i t e b) = freeVars (expandLet i t e b)
freeVars (Pi i k t)    = freeVars k `Set.union` freeVars t
freeVars (Kind _)      = Set.empty

type Context = Map String Type

extendContext :: String -> Type -> Context -> Context
extendContext = Map.insert

typeof :: Context -> Term -> Either TypeError (Term, Type)
typeof ctx b@TyBool                                       = Right (b, Var "Bool")
typeof ctx b@(BoolLit b')                                 = Right (b, Var "Bool")
typeof ctx i@(TmInt n)                                    = Right (i, Kind Star)
typeof ctx pm@(TmMatch e ((p, t) : branches))                               = do
    b1_ <- typeof ctx  t
    let (_, b1) = b1_
    b2 <- mapM (\(p, t) -> typeof ctx t) branches
    let branchNtypes = map snd b2
    if all (== b1) branchNtypes
    then return $ (pm, b1)
    else Left $ TypeError ("Match expression has a type error. ")

typeof ctx (Let s t a e)                                  = do
    typeof ctx t
    ta <- typeof ctx a
    let (_, u)                                            = ta
    when (not (betaEq u t)) $ Left $ TypeError $ "Bad let def\n" ++ show (u, t)
    te <- typeof ctx (subst s a e)
    let (_, u')                                           = te
    typeof ctx (Pi s t u')
    -- trace ("typeOf u' " ++ show u' ++ " ") (return ())
    return te
typeof ctx x@(Var v)                                      = case Map.lookup v ctx of
    Just ty -> Right (x, ty)
    Nothing -> Left $ TypeError (v ++ " " ++ "is not bound to any type.")
typeof ctx e@(TmAbs x ty t)                               = do
    _ <- typeof ctx ty
    let ctx'                                              = Map.insert x ty ctx
    ty_ <- typeof ctx' t
    let (_, ty')                                          = ty_
    let lt                                                = Pi x ty ty'
    _ <- typeof ctx lt
    return (e, lt)
typeof ctx e@(TmApp t1 t2)                                = do
    -- Typechek t1 in the context
    ty1 <- tCheckRed ctx t1
    case ty1 of
        -- (Π a:x . B) y
        -- at: argument type
        -- rt: return type
        Pi x at rt -> do
            -- Typechek t2 in the context
            ta' <- typeof ctx t2
            let (_, ta)                                   = ta'
            -- (betaEq x y)?
            -- "Since types can now be arbitrary expression
            -- we use betaEq to compare them instead of ( ==).""
            unless (betaEq ta at) $ Left $ TypeError ("Bad function arguments. Function: "
                                                      ++ show (nf t1) ++ " " ++
                                                      "argument: " ++ show (nf t2) ++ " " ++
                                                      "expected type: " ++ show at ++ " " ++
                                                      "got type: " ++ show ta)
            Right $ (e, subst x t2 rt)
        _ -> Left $ TypeError "Non-function in application"
typeof _ k@(Kind Star)                                    = return $ (k, Kind Box)
typeof _ (Kind Box)                                       = Left $ TypeError "Found box"
typeof ctx pi@(Pi x a b)                                  = do
    s <- tCheckRed ctx a
    let r'                                                = Map.insert x a ctx
    t <- tCheckRed r' b
    when ((s, t) `notElem` allowedKinds) $
        Left $
            TypeError $
                "Bad abstraction"
                    ++ show (s, t)
    return (pi, t)
typeof ctx_ t                                             = trace ("Uncaught pattern here: " ++ show t) (return (t, t))

allowedKinds :: [(Type, Type)]
allowedKinds =
    [ (Kind Star, Kind Star)
    , (Kind Star, Kind Box)
    , (Kind Box, Kind Star)
    , (Kind Box, Kind Box)
    ]

-- Type checks and normalizes the result.
tCheckRed r e = do
    let foo = typeof r e
    (e', t) <- foo
    return (whnf t)

whnf :: Term -> Term
whnf ee                          = spine ee []
  where
    spine (TmApp f a) as         = spine f (a : as)
    spine (TmAbs s _ e) (a : as) = spine (subst s a e) as
    spine (Let i t e b) as       = spine (expandLet i t e b) as
    spine f as                   = foldl TmApp f as

expandLet :: String -> Type -> Term -> Term -> Term
expandLet i t e b = TmApp (TmAbs i t b) e

subst :: String -> Term -> Term -> Term
subst v x = sub
  where
    sub e@(Var i) = if i == v then x else e
    sub (TmApp f a) = TmApp (sub f) (sub a)
    sub (TmAbs i t e) = abstr TmAbs i t e
    sub (Pi i t e) = abstr Pi i t e
    sub (TmInt n) = TmInt n
    sub TyBool = Kind Star
    sub (BoolLit b) = Var "Bool"
    sub TyInt = Kind Star
    sub (Let i t e b) =
        let TmApp (TmAbs i' t' b') e' = sub (expandLet i t e b)
         in Let i' t' e' b'
    sub (Kind k) = Kind k
    fvx = Set.toList $ freeVars x
    cloneSym e i = loop i
      where
        loop i' = if i' `elem` vars then loop (i ++ "'") else i'
        vars = fvx ++ Set.toList (freeVars e)
    abstr con i t e =
        if v == i
            then con i (sub t) e
            else
                if i `elem` fvx
                    then
                        let i' = cloneSym e i
                            e' = substVar i i' e
                         in con i' (sub t) (sub e')
                    else con i (sub t) (sub e)

betaEq :: Term -> Term -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

nf :: Term -> Term
nf ee = spine ee []
  where
    spine (TmApp f a) as         = spine f (a : as)
    spine (TmAbs s t e) []       = TmAbs s (nf t) (nf e)
    spine (TmAbs s _ e) (a : as) = spine (subst s a e) as
    spine (Let i t e b) as       = spine (TmApp (TmAbs i t b) e) as
    spine (Pi s k t) as          = app (Pi s (nf k) (nf t)) as
    spine f as                   = app f as
    app f as = foldl TmApp f (map nf as)

alphaEq :: Term -> Term -> Bool
alphaEq (Var v) (Var v') = v == v'
alphaEq (TmApp f a) (TmApp f' a') = alphaEq f f' && alphaEq a a'
alphaEq (TmAbs s _ e) (TmAbs s' _ e') = alphaEq e (substVar s' s e')
alphaEq (Let s t e b) (Let s' t' e' b') = alphaEq t t' && alphaEq e e' && alphaEq b (substVar s' s b')
alphaEq (Kind k) (Kind k') = k == k'
alphaEq (Pi s k t) (Pi s' k' t') = alphaEq k k' && alphaEq t (substVar s' s t')
alphaEq _ _ = False

substVar :: String -> String -> Term -> Term
substVar s s' = subst s (Var s')
