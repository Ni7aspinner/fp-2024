{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Chan
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict as S (StateT, evalStateT, get)
import Data.String.Conversions
import GHC.Conc (forkIO)
import Lib2 qualified
import Lib3 qualified
import Web.Scotty

type AppState = (TVar Lib2.State, Chan Lib3.StorageOp)


cmd :: String -> StateT AppState IO String
cmd "" = return "ERROR: Empty input received."
cmd str = do
  liftIO $ putStrLn $ "" 
  case Lib3.parseCommand str of
    Left e -> do
      liftIO $ putStrLn $ "cmd: Parse error: " ++ e
      return ("PARSE ERROR: " ++ e)
    Right (command, remainingStr) -> do
      liftIO $ putStrLn $ "cmd: Successfully parsed command: " ++ show command
      liftIO $ putStrLn $ "" 
      (st, chan) <- S.get
      liftIO $ putStrLn ""
      tr <- liftIO $ Lib3.stateTransition st command chan
      case tr of
        Left e2 -> do
          liftIO $ putStrLn $ "cmd: State transition error: " ++ e2
          return ("ERROR: " ++ e2)
        Right (Just msg, state) -> do
          liftIO $ putStrLn $ msg
          return state
        Right (Nothing, _) -> do
          liftIO $ putStrLn "cmd: State transition successful, no message."
          return ""

main :: IO ()
main = do
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  state <- newTVarIO Lib2.emptyState
  _ <- forkIO $ Lib3.storageOpLoop chan
  putStrLn "Server is starting"
  scotty 3000 $
    post "/" $ do
      b <- body
      liftIO $ putStrLn ("\nRequest was: " ++ cs b)
      result <- liftIO $ try (evalStateT (cmd (cs b)) (state, chan)) :: ActionM (Either SomeException String)
      case result of
        Left ex -> do
          liftIO $ putStrLn $ "Error: " ++ show ex
          text "Response: Error occurred while processing your request."
        Right responseMsg -> do
          text $ cs responseMsg