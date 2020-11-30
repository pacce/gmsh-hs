{-# LANGUAGE TemplateHaskell #-}
module Test.Node where
import Test.QuickCheck.All

prop_rev :: [Int] -> [Int] -> Bool
prop_rev xs ys = reverse (xs ++ ys) == (reverse xs) ++ (reverse ys)

return []

runTests :: IO Bool
runTests = $quickCheckAll
