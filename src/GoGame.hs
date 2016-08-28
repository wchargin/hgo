{-# LANGUAGE NamedFieldPuns #-}
module GoGame (

      GoGame
      -- Direct fields
    , playerOrder
    , originalPlayers
      -- not currentPlayerQueue (internal state)
    , board
      -- not pastAndPresentBoards (internal state)
    , captures
    , komi
    , passes
      -- Computed properties
    , gameOver
    , nextPlayer

      -- Constructor
    , goGame

      -- Result types
    , MoveError(..)
    , Score(..)

      -- Interaction
    , play
    , resign
    , pass
    , score

    ) where

import Control.Monad (guard)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified GoBoard as GB

data GoGame p = GoGame {
                         playerOrder          :: ![p]
                         -- ^ The order of the players currently in the game.

                       , originalPlayers      :: ![p]
                         -- ^ All players ever in the game,
                         -- including those who have now resigned.

                       , currentPlayerQueue   :: ![p]
                         -- ^ When not everyone has resigned,
                         -- this is a list of the form @p : ps@,
                         -- where @p@ is the player whose turn it is
                         -- and @ps@ is the longest suffix of 'playerOrder'
                         -- that does not contain @p@
                         -- (assuming that the elements of @playerOrder@
                         -- are distinct).
                         -- Thus, after player @p@ plays,
                         -- the queue should be set to @ps@
                         -- if @ps@ is not empty;
                         -- otherwise, it should be set to @playerOrder@.
                         --
                         -- When everyone has resigned,
                         -- this is the empty list.

                       , board                :: !(GB.Board p)
                         -- ^ The current game board.

                       , pastAndPresentBoards :: !(Set.Set (GB.Board p))
                         -- ^ A set containing
                         -- the current game board
                         -- and all previous game boards.
                         -- This is used for ko calculations.

                       , captures             :: !(Map.Map p Int)
                         -- ^ The number of captures scored by each player.

                       , komi                 :: !(Map.Map p Rational)
                         -- ^ The komi/handicap awardded to each player.

                       , passes               :: !(Set.Set p)
                         -- ^ Whether each player's most recent move
                         -- was to 'pass'.
                       }
                       deriving (Eq, Show)

-- | Construct a Go game of the given dimensions
-- with the given ordered list of players
-- and the given set of komi.
-- The default komi, if not set, is zero for each player.
goGame :: Ord p => Int -> Int -> [p] -> Map.Map p Rational -> GoGame p
goGame w h ps komi = GoGame { playerOrder = ps
                            , originalPlayers = ps
                            , currentPlayerQueue = ps
                            , board = GB.board w h
                            , pastAndPresentBoards =
                                  Set.singleton (GB.board w h)
                            , captures = Map.fromList $ zip ps (repeat 0)
                            , komi = komi `Map.union`
                                  Map.fromList (zip ps $ repeat 0)
                            , passes = Set.empty
                            }

-- | Check whether the game is over.
gameOver :: Ord p => GoGame p -> Bool
gameOver game = noPlayers || allPassed
  where
    noPlayers = null $ playerOrder game
    allPassed = Set.fromList (playerOrder game) `Set.isSubsetOf` passes game

-- | Get the current player and the player queue after the given move.
-- Requires that the game not be over (check 'gameOver');
-- throws an error otherwise.
advanceQueue :: GoGame p -> (p, [p])
advanceQueue GoGame{currentPlayerQueue = [], playerOrder = p : ps} = (p, ps)
advanceQueue GoGame{currentPlayerQueue = p : ps } = (p, ps)
advanceQueue _ = error "invariant violation: game is over"

-- | Get the next player, or @Nothing@ if the game is over.
nextPlayer :: Ord p => GoGame p -> Maybe p
nextPlayer game = guard (not $ gameOver game) >> Just (fst $ advanceQueue game)

-- | An error indicating invalid user input
-- when making a move of some kind.
data MoveError = Occupied
                 -- ^ A player may not place a stone on an occupied point.
               | Suicide
                 -- ^ A player may not play a move
                 -- that would result in a group of their own stones
                 -- being captured.
                 -- Note that opponents' stones are captured
                 -- before the player's own stones,
                 -- so it is legal to place a stone
                 -- into a point with no liberties
                 -- if and only iff this results in
                 -- the immediate capture of one or more adjacent groups,
                 -- such that the newly placed stone now has a liberty.
               | Ko
                 -- ^ A player may not play a move
                 -- that causes the board to return to any previous state.
               | OutOfBounds
                 -- ^ A player may not place any stone out of the game bounds.
               | GameOver
                 -- ^ A player may not make any move once the game is over.
               deriving (Eq, Show)

-- | Place a stone for the current player at the given location.
-- Return the subsequent state of the game,
-- or a 'MoveError' if the move is invalid.
play :: Ord p => GB.Point -> GoGame p -> Either MoveError (GoGame p)
play pt game@GoGame{board, pastAndPresentBoards = boards, captures, passes}
  | not $ GB.inBoard board pt    = Left OutOfBounds
  | isJust $ GB.get pt board     = Left Occupied
  | GB.get pt board' /= Just who = Left Suicide
  | board' `Set.member` boards   = Left Ko
  | gameOver game                = Left GameOver
  | otherwise = Right $ game { currentPlayerQueue = queue'
                             , board = board'
                             , pastAndPresentBoards = Set.insert board' boards
                             , captures =
                                   Map.adjust (+ captureCount) who captures
                             , passes = Set.delete who passes
                             }
  where
    (captureCount, board') = GB.play pt who board
    (who, queue') = advanceQueue game

-- | Resign on behalf of the current player.
-- Return the subsequent state of the game,
-- or a 'MoveError' if the move is invalid.
-- (This can only happen if the game is already over.)
resign :: Ord p => GoGame p -> Either MoveError (GoGame p)
resign game
  | gameOver game = Left GameOver
  | otherwise = Right $ game { currentPlayerQueue = queue'
                             , playerOrder = List.delete who $ playerOrder game
                             , passes = Set.delete who $ passes game
                             }
  where
    (who, queue') = advanceQueue game

-- | Pass a turn on behalf of the current player.
-- Return the subsequent state of the game,
-- or a 'MoveError' if the move is invalid.
-- (This can only happen if the game is already over.)
pass :: Ord p => GoGame p -> Either MoveError (GoGame p)
pass game
  | gameOver game = Left GameOver
  | otherwise = Right $ game { currentPlayerQueue = queue'
                             , passes = Set.insert who $ passes game
                             }
  where
    (who, queue') = advanceQueue game

-- | A player's score, broken down into its components.
-- This need not be the final score
-- (though it will be if the game is over, of course).
data Score = Score { fromTerritory :: !Int
                   , fromCaptures  :: !Int
                   , fromKomi      :: !Rational
                   , total         :: !Rational
                   }
                   deriving (Eq, Show)

-- | Get the scores for each player.
-- The game doesn't need to be over to call this function.
score :: Show p => Ord p => GoGame p -> Map.Map p Score
score GoGame{originalPlayers, board, captures, komi} =
    Map.fromList $ zip originalPlayers $ map scorePlayer originalPlayers
  where
    scorePlayer p =
        let t = Set.size $ Map.findWithDefault Set.empty p territories
            c = captures Map.! p
            k = komi Map.! p
            total = fromIntegral t + fromIntegral c + k
        in  Score { fromTerritory = t
                  , fromCaptures  = c
                  , fromKomi      = k
                  , total
                  }
    territories = GB.partition board
