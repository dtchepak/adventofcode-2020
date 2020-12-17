module Aoc.Day13Spec (spec) where

import Aoc.Day13
import Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    specify "example" $ do
      earliest 939 [7, 13, 59, 31, 19] `shouldBe` Just 59
  describe "part 2" $ do
    specify "main example" $ do
      consecutive "7,13,x,x,59,x,31,19" `shouldBe` Just 1068781

    specify "The earliest timestamp that matches the list 17,x,13,19 is 3417" $ do
      consecutive "17,x,13,19" `shouldBe` Just 3417

    specify "67,7,59,61 first occurs at timestamp 754018." $ do
      consecutive "67,7,59,61" `shouldBe` Just 754018

    specify "67,x,7,59,61 first occurs at timestamp 779210." $ do
      consecutive "67,x,7,59,61" `shouldBe` Just 779210

    specify "67,7,x,59,61 first occurs at timestamp 1261476." $ do
      consecutive "67,7,x,59,61" `shouldBe` Just 1261476

    specify "1789,37,47,1889 first occurs at timestamp 1202161486." $ do
      consecutive "1789,37,47,1889 " `shouldBe` Just 1202161486
