module Eval where

import qualified Data.Map as Map
import Control.Exception

import LispVal

lst :: (a,b) -> b
lst (a,b) = b

primitives :: Env
primitives = Map.fromList [("u", mkF $ numericOp (+)),
              ("-", mkF $ numericOp (-)),
              ("*", mkF $ numericOp (*)),
              ("+", mkF $ numericOp (+)),
              ("/", mkF $ numericOp (div)),
              ("mod", mkF $ numericOp (mod)),
              ("quotient", mkF $ numericOp (quot)),
              ("remainder", mkF $ numericOp (rem)),
              ("<", mkF $ comparisonOp (<)),
              ("<=", mkF $ comparisonOp (<=)),
              (">", mkF $ comparisonOp (>)),
              (">=", mkF $ comparisonOp (>=)),
              ("==", mkF $ comparisonOp (==))
              ]

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF func = Func $ IFunc func

apply :: String -> [LispVal] -> Env -> Eval Pair
apply func lvals env = case Map.lookup func env of 
    Just func -> case func of
            Func (IFunc internalF) -> do
                evaled <- internalF lvals
                return (env, evaled)
            _ -> throw $ FError "function doesn't exist"
    Nothing -> throw $ FError "function doesn't exist"

numericOp :: (Int -> Int -> Int) -> [LispVal] -> Eval LispVal
numericOp op args = return $ Number $ foldl1 op $ Prelude.map unpackNum args 

comparisonOp :: (Int -> Int -> Bool) -> [LispVal] -> Eval LispVal
comparisonOp op args = case length args of 
    2 -> return $ Bool $ op (unpackNum (args !! 0)) (unpackNum (args !! 1))
    _ -> throw $  TooManyArgs $ show args

unpackNum :: LispVal -> Int
unpackNum (Number n) = n

-- ************** EVAL ****************

eval :: Env -> LispVal -> Eval Pair
eval env (Number i) = return (env, Number i)
eval env (String s) = return (env, String s)
eval env (Bool b)   = return (env, Bool b)
eval env (List [])  = return (env, Nil)
eval env Nil        = return (env, Nil)

eval env (List [Atom "define", Atom var, expr]) = do
    evaled <- eval env expr
    return ((Map.insert var (lst evaled) env), (lst evaled)) 

eval env (Atom a) = case (Map.lookup a env) of 
    Just i -> return (env, i)
    Nothing -> throw $ VarNotExists a

eval env (List [Atom "if", cond, trueExpr, falseExpr]) = do
    ifRes <- eval env cond
    case (lst ifRes) of 
        (Bool True)  -> eval env trueExpr
        (Bool False) -> eval env falseExpr

eval env (List ((Atom "car") :[List ys])) = do
    evaled <- eval env $ head ys
    return $ evaled

eval env (List ((Atom "cdr") :[List ys])) = do
    evaled <- Prelude.mapM (eval env) (tail ys)
    let lvals = Prelude.map lst evaled in 
        return (env, List lvals)

eval env (List (Atom func : args)) =  do
    pairs <-  Prelude.mapM (eval env) args 
    let lvals = Prelude.map lst pairs in 
        apply func lvals env
    
eval env _ = throw $ FError "function doesn't exist"

