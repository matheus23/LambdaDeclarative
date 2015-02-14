module LambdaCalculus where

data LamExpr
  = Var String
  | Lam String LamExpr
  | App LamExpr LamExpr deriving (Show, Eq)

type Env = [(String, LamExpr)]

trueLam = Lam "t" $ Lam "f" $ Var "t"

falseLam = Lam "x" $ Lam "y" $ Var "y"

notLam = Lam "b" $ Var "b" `App` falseLam `App` trueLam

notLam2 = Lam "b" $ Lam "j" $ Lam "k" $ Var "b" `App` Var "k" `App` Var "j"

eval :: Env -> LamExpr -> LamExpr
eval env (Var str) = case lookup str env of
  Just expr -> expr
  Nothing   -> Var str
eval env (Lam var body) = Lam var $ eval env body
eval env (App func arg) = apply env (eval env func) (eval env arg)

apply :: Env -> LamExpr -> LamExpr -> LamExpr
apply env (Lam var body) arg = eval ((var, arg):env) body
apply env func arg = App func arg
