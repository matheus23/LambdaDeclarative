module LambdaCalculus2 where

data LamExpr
  = Var String
  | Lam String LamExpr
  | App LamExpr LamExpr deriving (Show, Eq)

data LValue = Func Env String LamExpr deriving (Show, Eq)

type Env = [(String, LValue)]

trueLam = Lam "t" $ Lam "f" $ Var "t"

falseLam = Lam "x" $ Lam "y" $ Var "y"

notLam = Lam "b" $ Var "b" `App` falseLam `App` trueLam

notLam2 = Lam "b" $ Lam "j" $ Lam "k" $ Var "b" `App` Var "k" `App` Var "j"

eval :: Env -> LamExpr -> LValue
eval env (Var str) = case lookup str env of
  Just expr -> expr
  Nothing   -> error $ "Couldn't find var: " ++ str ++ " in env: " ++ show env
eval env (Lam var body) = Func env var body
eval env (App func arg) = apply (eval env func) (eval env arg)

apply :: LValue -> LValue -> LValue
apply (Func env var body) arg = eval ((var, arg):env) body
