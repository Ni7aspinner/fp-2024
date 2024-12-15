{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Data.IORef (newIORef, readIORef, IORef, writeIORef)
import Control.Monad.Trans.State 
import Control.Monad.Free (Free(..), liftF)
import Lib2 qualified 
import Lib3 qualified 
import Lib1 
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests]
unitTests :: TestTree
unitTests = testGroup "Lib2 and Lib3 tests"
  [ 
    testGroup "InMemory DSL Tests"
      [ testCase "Add a single plan" $ do
          storage <- newIORef []
          (_, finalState) <- runStateT (Lib1.runInMemory storage $ add "Monday 3x2 squats") []
          finalState @?= [("Monday 3x2 squats", "active")]

      , testCase "Delete a plan" $ do
          storage <- newIORef []
          let program = do
                add "Monday 3x2 squats"
                delete 1
          (_, finalState) <- runStateT (runInMemory storage program) []
          finalState @?= []

      , testCase "Merge two plans" $ do
          storage <- newIORef []
          let program = do
                add "Monday 3x2 squats"
                add "Tuesday 3x2 squats"
                merge 1 2
          (_, finalState) <- runStateT (runInMemory storage program) []
          finalState @?= [("Monday 3x2 squats;\n   Tuesday 3x2 squats", "     [active]")]

      , testCase "Save and load plans" $ do
          storage <- newIORef []
          let program = do
                add "Monday 3x2 squats"
                save
                delete 1
                load
          (_, finalState) <- runStateT (runInMemory storage program) []
          finalState @?= [("Monday 3x2 squats", "active")]
      ]
  ]
instance Arbitrary Lib3.Statements where
    arbitrary = 
         Lib3.Batch <$> arbitrary
        

instance Arbitrary Lib2.Query where
    arbitrary = oneof
        [ return Lib2.List
        , Lib2.Add <$> arbitrary
        ]

instance Arbitrary Lib2.Plan where
    arbitrary = do
        days <- elements ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
        numRoutines <- choose (1,1) 
        routines <- vectorOf numRoutines arbitrary  
        let dayRoutines = map (\routine -> (days, routine)) routines 
        return $ Lib2.WeekDay dayRoutines

instance Arbitrary Lib2.Routine where
    arbitrary = Lib2.Routine <$> listOf1 arbitrary

instance Arbitrary Lib2.Exercise where
    arbitrary = do
        name <- listOf1 $ elements ['a'..'z'] 
        sor <- arbitrary  
        return $ Lib2.Exercise sor name
        
instance Arbitrary Lib2.SOR where
    arbitrary = do
        sets <- (choose (1, 10) :: Gen Int)  
        reps <- (choose (1, 10) :: Gen Int) 
        name <- arbitrary           
        return $ Lib2.SOR (show sets) (show reps) name

propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ QC.testProperty "parseStatements . renderStatements == Right statements" $ \statements ->
      Lib3.parseStatements (Lib3.renderStatements statements) == Right (statements,"")
  ]

add :: String -> MyDomain ()
add plan = liftF $ Add plan ()

delete :: Int -> MyDomain ()
delete index = liftF $ Delete index ()

merge :: Int -> Int -> MyDomain ()
merge index1 index2 = liftF $ Merge index1 index2 ()

list :: MyDomain String
list = liftF $ Lib1.List id

load:: MyDomain ()
load = liftF $ Load ()

save:: MyDomain ()
save = liftF $ Save ()