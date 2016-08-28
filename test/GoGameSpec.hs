module GoGameSpec (spec) where

import Test.Hspec (
      Spec
    , Expectation
    , shouldBe
    , shouldSatisfy
    , shouldNotSatisfy
    , expectationFailure
    , describe
    , context
    , it
    )

import Control.Monad
import Data.Either (isRight)
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Set as Set

import GoGame
import qualified GoBoard as GB
import TestUtil

{-# ANN module "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = do

    describe "goGame" $ do

        it "should initialize a standard configured game" $
            let n = 19
                ps = [X, O]
                komiMap = Map.fromList [(X, 0), (O, 6 + 1 % 2)]
                game = goGame n n ps komiMap
            in  ( playerOrder game
                , originalPlayers game
                , board game
                , captures game
                , komi game
                , passes game
                ) `shouldBe`
                ( [X, O]
                , [X, O]
                , GB.board n n
                , Map.fromList [(X, 0), (O, 0)]
                , Map.fromList [(X, 0), (O, 6 + 1 % 2)]
                , Set.empty
                )

        it "should let komi default to zero" $
            let game = goGame 19 19 [X, O] Map.empty
            in  komi game `shouldBe` Map.fromList [(X, 0), (O, 0)]

    describe "play, pass, and resign" $ do

        it "should place a piece in normal play" $
            let g0 = return $ goGame 5 5 [X, O] Map.empty
                g1 = g0 >>= play (GB.Point 3 3)
                expected = boardFromAscii xo
                    [ "....."
                    , "....."
                    , "..X.."
                    , "....."
                    , "....."
                    ]
            in  fmap board g1 `shouldBe` Right expected

        it "should alternate players when placing pieces" $
            let g0 = return $ goGame 5 5 [X, O] Map.empty
                g1 = g0
                    >>= play (GB.Point 3 3)
                    >>= play (GB.Point 1 2)
                expected = boardFromAscii xo
                    [ "....."
                    , "....."
                    , "..X.."
                    , "O...."
                    , "....."
                    ]
            in  fmap board g1 `shouldBe` Right expected

        it "should capture pieces" $
            let g0 = return $ goGame 5 5 [X, O] Map.empty
                g1 = g0
                    >>= play (GB.Point 1 2)
                    >>= play (GB.Point 1 1)
                    >>= play (GB.Point 2 1)
                expectedBoard = boardFromAscii xo
                    [ "....."
                    , "....."
                    , "....."
                    , "X...."
                    , ".X..."
                    ]
                expectedCaptures = Map.fromList [(X, 1), (O, 0)]
            in  fmap (liftM2 (,) board captures) g1
                `shouldBe` Right (expectedBoard, expectedCaptures)

        it "should allow would-be suicide to capture a stone" $
            let g0 = return $ goGame 5 5 [X, O] Map.empty
                g1 = g0
                    >>= play (GB.Point 2 1)
                    >>= play (GB.Point 1 1)
                    >>= play (GB.Point 1 5)
                    >>= play (GB.Point 3 1)
                    >>= play (GB.Point 2 5)
                    >>= play (GB.Point 2 2)
                expectedBoard = boardFromAscii xo
                    [ "XX..."
                    , "....."
                    , "....."
                    , ".O..."
                    , "O.O.."
                    ]
                expectedCaptures = Map.fromList [(X, 0), (O, 1)]
            in  fmap (liftM2 (,) board captures) g1
                `shouldBe` Right (expectedBoard, expectedCaptures)

        it "shouldn't allow non-capturing suicide" $
            let g0 = return $ goGame 5 5 [X, O] Map.empty
                g1 = g0
                    >>= play (GB.Point 2 1)
                    >>= play (GB.Point 2 2)
                    >>= play (GB.Point 1 2)
                g2 = g1
                    >>= play (GB.Point 1 1)
                expectedBoard = boardFromAscii xo
                    [ "....."
                    , "....."
                    , "....."
                    , "XO..."
                    , ".X..."
                    ]
            in  (fmap board g1, g2) `shouldBe`
                (Right expectedBoard, Left Suicide)

        it "shouldn't allow playing at occupied squares" $
            let g0 = return $ goGame 5 5 [X, O] Map.empty
                g1 = g0
                    >>= play (GB.Point 3 3)
                    >>= play (GB.Point 3 3)
            in  g1 `shouldBe` Left Occupied

        it "shouldn't allow playing out of bounds" $
            play (GB.Point 5 5) (goGame 3 3 [X, O] Map.empty)
            `shouldBe` Left OutOfBounds

        it "shouldn't allow playing on another player's stone" $
            let g0 = return $ goGame 5 5 [X, O] Map.empty
                g1 = g0 >>= play (GB.Point 1 1)
                g2 = g1 >>= play (GB.Point 1 1)
            in  (isRight g1, g2) `shouldBe` (True, Left Occupied)

        it "shouldn't allow playing on one's own stone" $
            let g0 = return $ goGame 5 5 [X, O] Map.empty
                g1 = g0 >>= play (GB.Point 1 1) >>= play (GB.Point 2 2)
                g2 = g1 >>= play (GB.Point 1 1)
            in  (isRight g1, g2) `shouldBe` (True, Left Occupied)

        it "should enforce the rule of ko" $
            let g0 = return $ goGame 5 3 [X, O] Map.empty
                g1 = g0
                    -- X on the left column;   O on the right
                    >>= play (GB.Point 1 2) >>= play (GB.Point 2 2)
                    >>= play (GB.Point 2 1) >>= play (GB.Point 3 1)
                    >>= play (GB.Point 5 3)
                b1 = Right $ boardFromAscii xo [ "....X"
                                               , "XO..."
                                               , ".XO.."
                                               ]
                g2 = g1 >>= play (GB.Point 1 1)
                b2 = Right $ boardFromAscii xo [ "....X"
                                               , "XO..."
                                               , "O.O.."
                                               ]
                -- X might like to play at (2, 1),
                -- but this would return the state to 'g1',
                -- so the move is disallowed.
                g3 = g2 >>= play (GB.Point 2 1)
                b3 = Left Ko
            in  map (fmap board) [g1, g2, g3] `shouldBe` [b1, b2, b3]

        it "should enforce the rule of superko" $
            -- We set up a scenario where
            -- the board never returns to its _immediately_ previous state,
            -- and yet the players would have it return to an earlier state
            -- (from eight moves ago).
            -- This is done by having multiple ko fights
            -- and alternating moves among them.
            let g0 = return $ goGame 5 5 [X, O] Map.empty
                g1 = g0
                    -- X on the left column ;   O on the right
                    >>= play (GB.Point 1 2) >>= play (GB.Point 2 2)
                    >>= play (GB.Point 2 1) >>= play (GB.Point 3 1)
                    >>= play (GB.Point 4 1) >>= play (GB.Point 4 2)
                    >>= play (GB.Point 5 2) >>= play (GB.Point 1 4)
                    >>= play (GB.Point 2 4) >>= play (GB.Point 2 5)
                    >>= play (GB.Point 3 5) >>= play (GB.Point 4 5)
                    >>= play (GB.Point 4 4) >>= play (GB.Point 5 4)
                b1 = Right $ boardFromAscii xo [ ".OXO."
                                               , "OX.XO"
                                               , "....."
                                               , "XO.OX"
                                               , ".XOX."
                                               ]
                -- It's now X's turn.
                g2 = g1
                    >>= play (GB.Point 1 5) >>= play (GB.Point 1 1)
                    >>= play (GB.Point 5 5) >>= play (GB.Point 5 1)
                b2 = Right $ boardFromAscii xo [ "X.X.X"
                                               , "OX.XO"
                                               , "....."
                                               , "XO.OX"
                                               , "O.O.O"
                                               ]
                -- Again, it's X's turn.
                -- Start taking them back.
                g3 = g2
                    >>= play (GB.Point 2 1) >>= play (GB.Point 2 5)
                    >>= play (GB.Point 4 1)
                b3 = Right $ boardFromAscii xo [ ".OX.X"
                                               , "OX.XO"
                                               , "....."
                                               , "XO.OX"
                                               , ".XOX."
                                               ]
                -- Now, O may not play at (4, 5)
                -- because this would return the game to 'g1'.
                g4 = g3 >>= play (GB.Point 4 5)
                b4 = Left Ko
            in  map (fmap board) [g1, g2, g3, g4] `shouldBe` [b1, b2, b3, b4]

        it "shouldn't allow playing once the game is over" $
            let g0 = return $ goGame 5 5 [X, O] Map.empty
                g1 = g0 >>= pass >>= pass
                g2 = g1 >>= pass
            in  (isRight g0, g2) `shouldBe` (True, Left GameOver)

        context "should remove resigned players from the queue" $ do

            it "under normal play" $
                let g0 = return $ goGame 5 5 [A, B, C] Map.empty
                    g1 = g0 >>= play (GB.Point 1 1)
                    g2 = g1 >>= resign             
                    g3 = g2 >>= play (GB.Point 2 2)
                    g4 = g3 >>= play (GB.Point 3 3)
                    g5 = g4 >>= play (GB.Point 4 4)
                    g6 = g5 >>= play (GB.Point 5 5)
                in  map (fmap nextPlayer) [g0, g1, g2, g3, g4, g5, g6]
                    `shouldBe` map (Right . Just) [A, B, C, A, C, A, C]

            it "even when the player to resign is last in the queue" $
                -- ...because then we cycle the list,
                -- so we should be careful
                -- (even though there actually isn't any extra logic).
                let g0 = return $ goGame 5 5 [A, B, C] Map.empty
                    g1 = g0 >>= play (GB.Point 1 1)
                    g2 = g1 >>= play (GB.Point 2 2)
                    g3 = g2 >>= resign             
                    g4 = g3 >>= play (GB.Point 3 3)
                    g5 = g4 >>= play (GB.Point 4 4)
                    g6 = g5 >>= play (GB.Point 5 5)
                in  map (fmap nextPlayer) [g0, g1, g2, g3, g4, g5, g6]
                    `shouldBe` map (Right . Just) [A, B, C, A, B, A, B]

    describe "gameOver" $ do

        it "should be true for a new game with no players" $
            gameOver (goGame 19 19 [] Map.empty :: GoGame XO) `shouldBe` True

        it "should be false for a new game with two players" $
            gameOver (goGame 19 19 [X, O] Map.empty) `shouldBe` False

        it "should be false if just one of two players passes" $
            let game = pass (goGame 19 19 [X, O] Map.empty)
            in  either setupFailed (`shouldNotSatisfy` gameOver) game

        it "should be true if all players pass" $
            let game = pass (goGame 19 19 [X, O] Map.empty) >>= pass
            in  either setupFailed (`shouldSatisfy` gameOver) game

        it "should be false if players pass non-consecutively" $
            let g0 = return $ goGame 19 19 [X, O] Map.empty
                g1 = g0
                    >>= pass
                    >>= play (GB.Point 1 1)
                    >>= play (GB.Point 1 2)
                    >>= pass
            in  either setupFailed (`shouldNotSatisfy` gameOver) g1

        it "should be false if one player passes twice" $
            let g0 = return $ goGame 19 19 [X, O] Map.empty
                g1 = g0 >>=
                     pass >>=
                     play (GB.Point 1 1) >>=
                     pass >>=
                     play (GB.Point 1 2)
            in  either setupFailed (`shouldNotSatisfy` gameOver) g1

    describe "score" $ do

        it "should just count komi at the beginning of the game" $
            let g = goGame 19 19 [X, O] $ Map.singleton O k
                k = 6 + 1 % 2
                expected = Map.fromList [ (X, Score 0 0 0 0)
                                        , (O, Score 0 0 k k)
                                        ]
            in  score g `shouldBe` expected

        it "should score a simple game" $
            let g0 = return $ goGame 5 5 [X, O] $ Map.singleton O k
                k = 6 + 1 % 2
                --
                -- We'll create the following board:
                b1 = Right $ boardFromAscii xo [ ".O..."
                                               , ".O..X"
                                               , "O..X."
                                               , ".OX.."
                                               , "OXX.."
                                               ]
                g1 = g0
                    >>= play (GB.Point 1 2) >>= play (GB.Point 2 2)
                    >>= play (GB.Point 2 1) >>= play (GB.Point 1 3)
                    >>= play (GB.Point 3 1) >>= play (GB.Point 1 1)
                    >>= play (GB.Point 4 3) >>= play (GB.Point 2 4)
                    >>= play (GB.Point 5 4) >>= play (GB.Point 2 5)
                    >>= play (GB.Point 3 2)
                expected = Map.fromList [ (X, Score 5 0 0 5)
                                        , (O, Score 3 1 k (k + 4))
                                        ]
            in  (fmap board g1, fmap score g1) `shouldBe` (b1, Right expected)

setupFailed :: MoveError -> Expectation
setupFailed e = expectationFailure $ "Setup failed: " ++ show e
