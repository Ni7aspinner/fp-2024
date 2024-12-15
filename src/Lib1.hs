{-# LANGUAGE DeriveFunctor #-}
module Lib1
    ( completions,
    runInMemory,
    MyDomain(..),
    Command(..)
    ) where
import Data.IORef 
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.Trans.State ( StateT, get, modify, put )
import Control.Monad.IO.Class


completions :: [String]
completions = [ "Add", "Delete", "List", "Merge"
    ,"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", ""
    , "x"
    , "Superset["
    , "leg-extensions", "squats", "pull-ups", "bench-press", "deadlift"
    , "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"  
    ]

data Command next =
    Add String next
    | Delete Int next
    | Merge Int Int next
    | List (String -> next)
    | Load next  
    | Save next
    deriving Functor

type MyDomain = Free Lib1.Command
type InMemoryState = [(String, String)]
type PersistentStorage = IORef InMemoryState

runInMemory :: PersistentStorage -> MyDomain a -> StateT InMemoryState IO a
runInMemory storage (Pure a) = return a
runInMemory storage (Free step) = case step of
    Add plan next -> do
        modify (\s -> s ++ [(plan, "active")])
        runInMemory storage next

    Delete index next -> do
        modify (\s -> if index > 0 && index <= length s
                        then take (index - 1) s ++ drop index s
                        else s)
        runInMemory storage next

    Merge index1 index2 next -> do
        currentState <- get
        let mergedPlan = case (getPlan index1 currentState, getPlan index2 currentState) of
                (Just (p1, _), Just (p2, _)) -> Just (p1 ++ ";\n   " ++ p2, "     [active]")
                _ -> Nothing
        let newState = mergePlans index1 index2 currentState mergedPlan
        put newState
        runInMemory storage next

    List next -> do
        currentState <- get
        let output = unlines $ zipWith formatPlan [1..] currentState
        liftIO $ putStrLn "Created Plans:"
        liftIO $ putStrLn output
        runInMemory storage (next output)

    Save next -> do
        currentState <- get
        liftIO $ writeIORef storage currentState
        liftIO $ putStrLn "State saved to persistent storage."
        runInMemory storage next

    Load next -> do
        savedState <- liftIO $ readIORef storage
        put savedState
        liftIO $ putStrLn "State loaded from persistent storage."
        runInMemory storage next

getPlan :: Int -> [(String, String)] -> Maybe (String, String)
getPlan idx state = if idx > 0 && idx <= length state then Just (state !! (idx - 1)) else Nothing

mergePlans :: Int -> Int -> [(String, String)] -> Maybe (String, String) -> [(String, String)]
mergePlans idx1 idx2 state (Just merged) =
    let newState = [state !! i | i <- [0 .. length state - 1], i /= idx1 - 1, i /= idx2 - 1]
    in [merged] ++ newState 
mergePlans _ _ state Nothing = state

formatPlan :: Int -> (String, String) -> String
formatPlan idx (desc, status) = show idx ++ ". " ++ desc ++ " " ++ status