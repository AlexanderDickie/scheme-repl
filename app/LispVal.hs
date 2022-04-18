{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispVal where 

import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Except 
import Control.Monad.Reader
import Control.Exception

type Env = Map.Map String LispVal
type Eval a = IO a
type Pair = (Env, LispVal)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }
-- or data IFunc = IFunc ([LispVal] -> Eval LispVal)
-- eg a haskell function ([LispVal] -> Eval LispVal) to an IFunc

data LispVal =
    Atom String
    | List [LispVal]
    | Number Int
    | String String
    | Func IFunc    --primitive function
    | Lambda IFunc Env  -- user defined function
    | Nil
    | Bool Bool 

instance Show LispVal where show = showLisp

data LispError = 
    TooManyArgs String |
    VarNotExists String |
    PError String |
    FError String
    deriving Show

instance Exception LispError 

showLisp :: LispVal -> String 
showLisp x = case x of 
    (Atom x) -> x  
    (List x) -> "(" ++ (rmSpace $ concat $ showLisp <$> x) ++ ")"
    (Number x) -> show x ++ " " 
    (String x) -> x 
    (Func x) -> "Primitive Function"  
    (Lambda x _) -> "User Defined Function"   
    Nil -> "Nil" 
    (Bool True) -> "#t"
    (Bool False) -> "#f"

showLispL :: LispVal -> String
showLispL x = showLisp x ++ " "

rmSpace :: String -> String
rmSpace [x] = ""
rmSpace (x:xs) = [x] ++ rmSpace xs

basicEnv :: Env 
basicEnv = Map.empty 