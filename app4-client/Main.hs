{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Data.ByteString
import Network.Wreq
import Control.Monad.State
import Data.String.Conversions
import Control.Lens hiding (List)
import Control.Monad.Free (Free (..), liftF)
import Data.IORef
import System.Environment (getArgs)
import qualified Lib3 
import qualified Lib3 

data Command next =
    Add String next
    | Delete Int next
    | Merge Int Int next
    | List (String -> next)
    | Load next  
    | Save next
    deriving Functor

type MyDomain = Free Command

add :: String -> MyDomain ()
add plan = liftF $ Add plan ()

delete :: Int -> MyDomain ()
delete index = liftF $ Delete index ()

merge :: Int -> Int -> MyDomain ()
merge index1 index2 = liftF $ Merge index1 index2 ()

list :: MyDomain String
list = liftF $ Main.List id

load:: MyDomain ()
load = liftF $ Load ()

save:: MyDomain ()
save = liftF $ Save ()

runHttp :: MyDomain a -> IO a
runHttp (Pure a) = return a
runHttp (Free step) = do
  next <- runStep step
  runHttp next
  where
    runStep :: Command a -> IO a
    runStep (Load next) = do
      let rawRequest = cs "Load" :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next 
    runStep (Add plan next) = do
      let rawRequest = cs ("Add " ++ plan ) :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next 
    runStep (Delete index next) = do
      let rawRequest = cs ("Delete " ++ show index) :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next 
    runStep (Merge index1 index2 next) = do
      let rawRequest = cs ("Merge " ++ show index1 ++ " " ++ show index2) :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next 
    runStep (List next) = do
      let rawRequest = cs "List" :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      putStrLn $ cs $ resp ^. responseBody
      return $ next (cs $ resp ^. responseBody)
    runStep (Save next) = do
      let rawRequest = cs "Save" :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next 


runHttpBatch :: MyDomain a -> IO a
runHttpBatch commands = runStep commands (cs "BEGIN\n" :: ByteString)
  where
    runStep :: MyDomain a -> ByteString -> IO a
    runStep (Pure a) accumulatedCommands = do
        let finalCommands = accumulatedCommands `mappend` cs "END"
        putStrLn $ "" 
        return a

    runStep (Free cmd) accumulatedCommands = case cmd of
        Add plan next -> do
            let rawRequest = cs ("Add " ++ plan ++ "\n") :: ByteString
            runStep next (accumulatedCommands `mappend` rawRequest)

        Delete idx next -> do
            let rawRequest = cs ("Delete " ++ show idx ++ "\n") :: ByteString
            runStep next (accumulatedCommands `mappend` rawRequest)

        Merge i1 i2 next -> do
            let rawRequest = cs ("Merge " ++ show i1 ++ " " ++ show i2 ++ "\n") :: ByteString
            runStep next (accumulatedCommands `mappend` rawRequest)

        List next -> do
            sendBatch accumulatedCommands
            resp <- post "http://localhost:3000" (cs "List" :: ByteString)
            let receivedBody = cs $ resp ^. responseBody
            putStrLn $(cs $ resp ^. responseBody)
            runStep (next receivedBody) (cs "BEGIN\n" :: ByteString)

        Load next -> do
            sendBatch accumulatedCommands
            resp <- post "http://localhost:3000" (cs "Load" :: ByteString)
            runStep next (cs "BEGIN\n" :: ByteString) 

        Save next -> do
            sendBatch accumulatedCommands
            resp <- post "http://localhost:3000" (cs "Save" :: ByteString)
            runStep next (cs "BEGIN\n" :: ByteString) 

sendBatch :: ByteString -> IO ()
sendBatch accumulatedCommands = do
    resp <- post "http://localhost:3000" (accumulatedCommands `mappend` cs "END")
    putStrLn $ "" 

main :: IO ()
main = do
    args <- getArgs
    let program = do
            add "Monday 3x2 squats"
            add "Tuesday 3x2 squats, Superset[3x2 squats, 3x2 squats], 3x2 squats"
            merge 1 2
            list
            save
            Main.delete 1
            list
            load
            list
    case args of
        ["single"] -> do
            putStrLn "Running with HTTP single request per command:"
            output' <- runHttp program
            return ()
        ["batch"] -> do
            putStrLn "Running with HTTP batch or single (smart) request per command:"
            output' <- runHttpBatch program
            return ()
        _ -> putStrLn "Choose a DSL"