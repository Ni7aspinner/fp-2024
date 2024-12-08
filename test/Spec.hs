{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.QuickCheck as QC

import Lib2 qualified
import Lib3
import Debug.Trace
import qualified Lib2 as Lib3
import qualified Lib3 as Lib2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests]

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

      