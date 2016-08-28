module TestUtilSpec (spec) where

import Test.Hspec (Spec, shouldBe, describe, it)

import GoBoard
import TestUtil

spec :: Spec
spec = do

    describe "xo" $ do
        it "should send 'x' to player X"  $ xo 'x' `shouldBe` Just X
        it "should send 'X' to player X"  $ xo 'X' `shouldBe` Just X
        it "should send 'o' to player O"  $ xo 'o' `shouldBe` Just O
        it "should send 'O' to player O"  $ xo 'O' `shouldBe` Just O
        it "should send space to Nothing" $ xo ' ' `shouldBe` Nothing
        it "should send dot to Nothing"   $ xo '.' `shouldBe` Nothing

    describe "abcd" $ do
        it "should send 'a' to player A"  $ abcd 'a' `shouldBe` Just A
        it "should send 'A' to player A"  $ abcd 'A' `shouldBe` Just A
        it "should send 'b' to player B"  $ abcd 'b' `shouldBe` Just B
        it "should send 'B' to player B"  $ abcd 'B' `shouldBe` Just B
        it "should send 'c' to player C"  $ abcd 'c' `shouldBe` Just C
        it "should send 'C' to player C"  $ abcd 'C' `shouldBe` Just C
        it "should send 'd' to player D"  $ abcd 'd' `shouldBe` Just D
        it "should send 'D' to player D"  $ abcd 'D' `shouldBe` Just D
        it "should send space to Nothing" $ abcd ' ' `shouldBe` Nothing
        it "should send dot to Nothing"   $ abcd '.' `shouldBe` Nothing

    describe "boardFromAscii" $ do

        it "should create an empty board" $
            boardFromAscii xo [] `shouldBe` board 0 0

        it "should create a blank 3-by-2 board" $
            boardFromAscii xo ["---", "---"] `shouldBe` board 3 2

        it "should create a populated 3-by-2 board" $
            let b = boardFromAscii xo [ "xX."
                                      , ".Oo"]
                expectations = [ (Point 1 1, Nothing)
                               , (Point 2 1, Just O)
                               , (Point 3 1, Just O)
                               , (Point 1 2, Just X)
                               , (Point 2 2, Just X)
                               , (Point 3 2, Nothing)
                               ]
            in  mapM_ (\(pt, z) -> get pt b `shouldBe` z) expectations
