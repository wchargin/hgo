module GoBoardSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec (Spec, shouldBe, shouldNotBe, describe, context, it)

import GoBoard
import TestUtil

{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Redundant $" #-}

spec :: Spec
spec = do

    describe "get, set, and (==)" $ do

        it "should compare two equal-sized empty boards equal" $
            (board 5 7 :: Board ()) `shouldBe` board 5 7

        it "should compare two different-sized empty boards distinct" $
            (board 5 7 :: Board ()) `shouldNotBe` board 8 12

        context "should compare equal two boards with the same pieces" $ do

            it "using 'set'" $
                let b1 = (set (Point 3 4) (Just "Alice")) $
                         (set (Point 5 7) (Just "Bob")) $
                         board 19 19
                    b2 = (set (Point 5 7) (Just "Bob")) $
                         (set (Point 3 4) (Just "Alice")) $
                         board 19 19
                in  b1 `shouldBe` b2

            it "using 'put'" $
                let b1 = (put (Point 3 4) "Alice") $
                         (put (Point 5 7) "Bob") $
                         board 19 19
                    b2 = (put (Point 5 7) "Bob") $
                         (put (Point 3 4) "Alice") $
                         board 19 19
                in  b1 `shouldBe` b2

            it "mixing 'set' and 'put'" $
                let b1 = (set (Point 3 4) (Just "Alice")) $
                         (put (Point 5 7) "Bob") $
                         board 19 19
                    b2 = (set (Point 5 7) (Just "Bob")) $
                         (put (Point 3 4) "Alice") $
                         board 19 19
                in  b1 `shouldBe` b2

        it "shouldn't distinguish between Nothing and removed" $
            set (Point 3 4) Nothing (board 19 19 :: Board ())
            `shouldBe` board 19 19

        it "should 'get' a 'set' piece" $
            get (Point 3 4) (set (Point 3 4) (Just "Bob") (board 19 19))
            `shouldBe` Just "Bob"

        it "should 'get' a 'set' Nothing" $
            get (Point 3 4) (set (Point 3 4) Nothing $
                             set (Point 3 4) (Just "Bob") $
                             board 19 19)
            `shouldBe` Nothing

        it "should 'get' a 'put' piece" $
            get (Point 3 4) (put (Point 3 4) "Bob" (board 19 19))
            `shouldBe` Just "Bob"

        it "should 'get' Nothing from an unused point" $
            get (Point 3 8) (put (Point 3 4) "Bob" (board 19 19))
            `shouldBe` Nothing

    describe "group" $ do

        it "should identify a single stone as a group" $
            let b = boardFromAscii xo [ "...", ".x.", "..." ]
            in  group (Point 2 2) b `shouldBe` Set.singleton (Point 2 2)

        it "should identify a group of connected stones" $
            let b = boardFromAscii xo [ "xx."
                                      , ".x."
                                      , ".xx"
                                      ]
            in  group (Point 2 2) b `shouldBe` Set.fromList
                    [ Point 1 3
                    , Point 2 3
                    , Point 2 2
                    , Point 2 1
                    , Point 3 1
                    ]

        it "should not group together stones of different colors" $
            let b = boardFromAscii xo [ ".x."
                                      , "xox"
                                      , ".x."
                                      ]
            in  group (Point 2 2) b `shouldBe` Set.singleton (Point 2 2)

        it "should return the empty set for an unoccupied point" $
            let b = boardFromAscii xo [ ".x."
                                      , "xox"
                                      , ".x."
                                      ]
            in  group (Point 1 1) b `shouldBe` Set.empty

    describe "liberties" $ do

        it "should note that a surrounded group has no liberties" $
            let b = boardFromAscii xo [ ".x."
                                      , "xox"
                                      , ".x."
                                      ]
            in  liberties (Point 2 2) b `shouldBe` Set.empty

        it "should not count liberties off the board" $
            let b = boardFromAscii xo [ "x.x"
                                      , "xx."
                                      ]
                expected = Set.fromList [Point 2 2, Point 3 1]
            in  liberties (Point 1 1) b `shouldBe` expected

    describe "capture" $ do

        it "should capture a surrounded group" $
            let b = boardFromAscii xo [ ".x."
                                      , "xox"
                                      , ".x."
                                      ]
                b' = set (Point 2 2) Nothing b
            in  capture (Point 2 2) b `shouldBe` (1, b')

        it "should not capture a group with liberties" $
            let b = boardFromAscii xo [ ".x."
                                      , "xoo"
                                      , ".x."
                                      ]
            in  capture (Point 2 2) b `shouldBe` (0, b)

    describe "play" $ do

        it "should place a stone under normal play" $
            let b = boardFromAscii abcd [ ".aa."
                                        , "b..b"
                                        , ".c.c"
                                        , "d..."
                                        ]
                b' = put (Point 4 1) D b
            in  play (Point 4 1) D b `shouldBe` (0, b')

        it "should place a stone to capture" $
            let b = boardFromAscii abcd [ ".aa."
                                        , "b.ab"
                                        , ".cac"
                                        , "d.a."
                                        ]
                b' = set (Point 4 2) Nothing $ put (Point 4 1) A $ b
            in  play (Point 4 1) A b `shouldBe` (1, b')

        it "should place a stone in a no-liberty connected space" $
            let b = boardFromAscii abcd [ ".aa."
                                        , "b.ab"
                                        , ".cab"
                                        , "d.a."
                                        ]
                b' = put (Point 4 1) A b
            in  play (Point 4 1) A b `shouldBe` (0, b')

        it "should be able to capture a non-singleton group" $
            let b =  boardFromAscii abcd [ ".aaa"
                                         , "aaba"
                                         , "ccbb"
                                         , "c..a"
                                         ]
                b' = boardFromAscii abcd [ "d..."
                                         , "..b."
                                         , "ccbb"
                                         , "c..a"
                                         ]
            in play (Point 1 4) D b `shouldBe` (6, b')

        it "should be able to capture groups from multiple players" $
            let b =  boardFromAscii abcd [ ".aaa"
                                         , "bbba"
                                         , "ccbb"
                                         , "c.aa"
                                         ]
                b' = boardFromAscii abcd [ "d..."
                                         , "...."
                                         , "cc.."
                                         , "c.aa"
                                         ]
            in play (Point 1 4) D b `shouldBe` (9, b')

    describe "enclosure" $ do

        it "on an empty board, should identify all points with no owner" $
            let b = board 19 19 :: Board ()
                points = Set.fromList $ [ Point x y
                                        | x <- [1..19]
                                        , y <- [1..19]
                                        ]
            in  enclosure (Point 3 3) b `shouldBe` (Nothing, points)

        it "should identify a corner enclosure" $
            let b = boardFromAscii xo [ "xxx."
                                      , "..xx"
                                      , "...x"
                                      ]
                expected = Set.fromList $ map (uncurry Point)
                    [ (1, 1), (2, 1), (3, 1), (1, 2), (2, 2) ]
            in enclosure (Point 1 1) b `shouldBe` (Just X, expected)

        it "should identify a wall enclosure" $
            let b = boardFromAscii xo [ ".xxx."
                                      , ".x.xx"
                                      , ".x..x"
                                      ]
                expected = Set.fromList $ map (uncurry Point)
                    [ (3, 1), (3, 2), (4, 1)]
            in enclosure (Point 3 1) b `shouldBe` (Just X, expected)

        it "should not award a corner enclosure with an opposing piece" $
            let b = boardFromAscii xo [ "xxx."
                                      , "..xx"
                                      , "o..x"
                                      ]
                expected = Set.fromList $ map (uncurry Point)
                    [ (1, 2), (2, 2), (2, 1), (3, 1) ]
            in  enclosure (Point 1 2) b `shouldBe` (Nothing, expected)

        it "should award enclosures that contain non-contiguous borders" $
            let b = boardFromAscii xo [ "......"
                                      , "..oo.."
                                      , ".o..o."
                                      , ".o..o."
                                      , "..oo.."
                                      , "......"
                                      ]
                expected = Set.fromList $ map (uncurry Point)
                    [ (3, 3), (3, 4), (4, 3), (4, 4) ]
            in  enclosure (Point 3 3) b `shouldBe` (Just O, expected)

        it "should award an enclosure around the outside of the board" $
            let b = boardFromAscii xo [ "......"
                                      , "..oo.."
                                      , ".o..o."
                                      , ".o..o."
                                      , "..oo.."
                                      , "......"
                                      ]
                expected = Set.fromList $ concat
                    [ [ Point x y | x <- [1..6], y <- [1, 6] ]
                    , [ Point x y | x <- [1, 6], y <- [1..6] ]
                    , [ Point x y | x <- [2, 5], y <- [2, 5] ]
                    ]
            in  enclosure (Point 1 5) b `shouldBe` (Just O, expected)

        it "shouldn't award an almost-full-board region with opposing piece" $
            let b = boardFromAscii xo [ "x....."
                                      , "..oo.."
                                      , ".o..o."
                                      , ".o..o."
                                      , "..oo.."
                                      , "......"
                                      ]
                expected = Set.fromList $ concat
                    [ [ Point x y | x <- [1..6], y <- [1] ]
                    , [ Point x y | x <- [2..6], y <- [6] ]
                    , [ Point x y | x <- [1, 6], y <- [1..5] ]
                    , [ Point x y | x <- [2, 5], y <- [2, 5] ]
                    ]
            in  enclosure (Point 2 2) b `shouldBe` (Nothing, expected)
