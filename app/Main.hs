{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Exception 

import System.Console.Haskeline 

import Parse
import LispVal
import Eval

main :: IO ()
main = runRepl primitives repl

-- ************** Repl *************
type Repl a =  (ReaderT Env (InputT IO)) a

runRepl :: Env -> Repl () -> IO ()
runRepl env repl = runInputT defaultSettings (runReaderT repl env)

safeEval :: Env -> String -> Eval Pair
safeEval env s = either (throw . PError . show) (eval env) $ readExpr s 

handleErrs :: Eval Pair -> Eval (Either String Pair)
handleErrs m = do
    result <- Control.Exception.try m :: IO (Either LispError Pair)
    case result of 
        Left err -> return $ Left $ show err
        Right val -> return $ Right val

repl :: Repl ()
repl = do
  env <- ask
  inp <- lift $ getInputLine "Repl> " 
  case inp of 
        Nothing -> return ()
        Just "" -> lift $ outputStrLn "Bye"
        Just "quit" -> lift $ outputStrLn "Bye"

        Just str -> do
            evaled <- liftIO $ handleErrs $ safeEval env str
            case evaled of 
                Left err -> do
                    liftIO $ putStrLn err
                    local (const (env)) $ repl
                Right val -> do
                    liftIO $ putStrLn $ show $ (lst val)
                    local (const (fst val)) $ repl





