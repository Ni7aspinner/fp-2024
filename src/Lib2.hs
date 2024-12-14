{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Redundant case" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib2
    ( 
      Query(..),
      State(..),
      parseQuery,
      Plan(..),
      Routine(..),
      Exercise(..),
      SOR(..),
      emptyState,
      stateTransition
    ) where

import qualified Control.Monad.Trans.State.Strict as T
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)
import qualified Data.Char as C
import qualified Data.List as L
import Text.Read (readMaybe)

instance Eq Query where
      (==) :: Query -> Query -> Bool
      (==) q1 q2 = show q1 == show q2

instance Eq Plan where
      (==) :: Plan -> Plan -> Bool
      (==) q1 q2 = show q1 == show q2

-- <cline> ::= <command> <plan>
data Query = Add Plan | Delete Int | Merge Int Int | List 

-- <plan> ::= <weekDay> "(" <routine> ")" | <weekDay> "(" <routine>  ") " <plan> 
data Plan = WeekDay [(String, Routine)]

-- <routine> ::= <exercise> | <exercise> ", " <routine> 
data Routine = Routine [Exercise]

-- <exercise> ::= <sor> <name> | "Superset of [" <routine> "]"
data Exercise = Exercise SOR String | SuperSet Routine

-- <sor> ::= <digits> "x" <digits> "repetitions of "
data SOR = SOR String String Char

type Parser a = ExceptT String (T.State String) a


instance Show Query where
    show :: Query -> String
    show List = "List"
    show (Add plan) = "Add " ++ show plan
    show (Delete idx) = "Delete " ++ show idx
    show (Merge idx1 idx2) = "Merge " ++ show idx1 ++ " " ++ show idx2

instance Show Plan where
    show :: Plan -> String
    show (WeekDay days) = 
        let planStrs = map (\(day, routine) -> day ++ ": " ++ show routine) days
        in L.intercalate ";\n   " planStrs ++ ";"

instance Show Routine where
    show :: Routine -> String
    show (Routine exercises) = L.intercalate ", " (map show exercises)

instance Show Exercise where
    show :: Exercise -> String
    show (Exercise sor name) =  show sor  ++ name 
    show (SuperSet routine) = "Superset of [" ++ show routine ++ "]"

instance Show SOR where
    show :: SOR -> String
    show (SOR sets reps _) = sets ++ "x" ++ reps ++ " repetitions of "

instance Show State where
    show :: State -> String
    show (State created) =
        "Created Plans:\n" ++ listPlans created
      where
        listPlans :: [Plan] -> String
        listPlans = unlines . zipWith (\i plan -> show i ++ ". " ++ show plan) [1..]

parseQuery :: String -> Either String Query
parseQuery input = 
    case T.evalState (runExceptT (parseList `orElse` parseMerge `orElse` parseOrder)) input of
        Left err   -> Left err
        Right query -> Right query

parseList :: Parser Query
parseList = do
    input <- lift T.get
    case removeSpaces input of
        'L':'i':'s':'t':rest -> do
            lift $ T.put rest
            return List
        _ -> throwE "Expected 'List'"

parseOrder :: Parser Query
parseOrder = do
    input <- lift T.get
    case removeSpaces input of
        'A':'d':'d':' ':rest -> do
            let (planStr, remaining) = break (== ';') rest
            lift $ T.put planStr
            result <- lift $ runExceptT parsePlan 
            case result of
                Right plan -> do
                    lift $ T.put (drop 1 remaining)
                    return (Add plan)
                Left err -> throwE err

        'D':'e':'l':'e':'t':'e':' ':rest -> do
            let idxStr = takeWhile C.isDigit rest
            case readMaybe idxStr of
                Just idx -> do
                    lift $ T.put (dropWhile C.isDigit rest)
                    return (Delete idx)
                Nothing -> throwE "Invalid index for Delete"
        _ -> throwE "Expected 'Add <plan>' or 'Delete <number>'"

parseMerge :: Parser Query
parseMerge = do
    input <- lift T.get
    case removeSpaces input of
        'M':'e':'r':'g':'e':' ':rest -> do
            let (idx1Str, rest') = span C.isDigit rest
                (idx2Str, rest'') = span C.isDigit (removeSpaces rest')
            case (readMaybe idx1Str, readMaybe idx2Str) of
                (Just idx1, Just idx2) -> do
                    lift $ T.put rest''
                    return (Merge idx1 idx2)
                _ -> throwE "Invalid indices for Merge"
        _ -> throwE "Expected 'Merge <index1> <index2>'"

parsePlan :: Parser Plan
parsePlan =
    let days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
    in parseWeekDay days
    
parseWeekDay :: [String] -> Parser Plan
parseWeekDay [] = throwE "No valid weekday found"
parseWeekDay (day:rest) = do
    input' <- lift T.get
    if day `L.isPrefixOf` input'
    then do
        let remaining = drop (length day) input'
        lift $ T.put remaining
        routineResult <- lift $ runExceptT parseRoutine
        case routineResult of
            Right routine -> do
                return $ WeekDay [(day, routine)]  
            Left err -> throwE err
    else parseWeekDay rest

parseRoutine :: Parser Routine
parseRoutine  = do
    input <- lift T.get
    let parseExercises :: String -> Parser ([Exercise], String)
        parseExercises input' = do
            (exercise, rest1) <- parseExercise input'  
            let restTrimmed = removeSpaces rest1
            case parseCommaOrEnd restTrimmed of
                Right (Just ',', rest2) -> do
                    (moreExercises, restFinal) <- parseExercises rest2
                    return (exercise : moreExercises, restFinal)
                Right (Nothing, restFinal) ->
                    return ([exercise], restFinal)
                Left err -> throwE err

    (exercises, _) <- parseExercises input
    return (Routine exercises)

parseCommaOrEnd :: String -> Either String (Maybe Char, String)
parseCommaOrEnd input' =
    case removeSpaces input' of
        ',':' ':rest -> Right (Just ',', rest)
        _            -> Right (Nothing, input')

parseExercise :: String -> Parser (Exercise, String)
parseExercise input =
    case parseSOR (removeSpaces input) of
        Right (sor, rest) ->
            let (name, rest') = span (/= ',') (removeSpaces rest)
            in if null name
                then throwE "Expected exercise name after SOR"
                else return (Exercise sor name, rest')
        Left err ->
            if "Superset[" `L.isPrefixOf` input
            then
                let inner = drop (length "Superset[") input
                    inner' = takeWhile (/= ']') inner
                    rest' = drop (length inner') inner
                in do
                    lift $ T.put inner'
                    routine <- parseRoutine 
                    if "]" `L.isPrefixOf` rest'
                        then return (SuperSet routine, drop 1 rest') 
                        else throwE "Expected ']' to close Superset"
            else throwE err

parseSOR :: String -> Either String (SOR, String)
parseSOR input =
    let (sets, rest1) = span C.isDigit input
        rest2 = dropWhile (`elem` "xX") rest1 
        (reps, rest3) = span C.isDigit rest2
    in if null sets || null reps
       then Left $ "Invalid SOR format: Expected format <sets>x<reps>, but got '" ++ input ++ "'"
       else Right (SOR sets reps 'x', removeSpaces rest3)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = ExceptT $ do
    state <- T.get
    result <- runExceptT p1
    case result of
        Right res -> return(Right res)
        Left _ -> do
            T.put state 
            runExceptT p2
{-        
and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f a b c = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> Right (f v1 v2 v3, r3)
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1
-}

removeSpaces :: String -> String
removeSpaces = dropWhile C.isSpace

type CreatedPlans = [Plan]

data State = State CreatedPlans

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State []

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State created) query =
    case query of
        List -> Right (Just (show (State created)), State created)
        
        Add plan ->
            let newState = State (created ++ [plan])
            in Right (Just $ "Added Plan: " ++ show plan, newState)
        
        Merge idx1 idx2 -> 
            if idx1 < 1 || idx1 > length created || idx2 < 1 || idx2 > length created || idx1 == idx2
            then Left $ "Invalid indices: " ++ show idx1 ++ ", " ++ show idx2
            else
                let idx1' = min idx1 idx2 
                    idx2' = max idx1 idx2
                    (before1, plan1 : after1) = splitAt (idx1' - 1) created
                    (before2, plan2 : after2) = splitAt (idx2' - idx1' - 1) after1
                    mergedPlan = mergePlans plan1 plan2
                    newState = State (before1 ++ [mergedPlan] ++ before2 ++ after2)
                in Right (Just $ "Merged Plans: " ++ show idx1 ++ " and " ++ show idx2, newState)
        
        Delete idx ->
            if idx < 1 || idx > length created
            then Left $ "Invalid index: " ++ show idx
            else 
                let (before, toDelete : after) = splitAt (idx - 1) created  
                in Right (Just $ "Deleted Plan: " ++ show toDelete, State (before ++ after)) 

mergePlans :: Plan -> Plan -> Plan
mergePlans (WeekDay days1) (WeekDay days2) = WeekDay (days1 ++ days2)