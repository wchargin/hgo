{-# LANGUAGE DeriveFunctor, TupleSections #-}
module GoBoard (

      Point(..)
    , inBounds
    , inBoard

    , Board
    , board
    , width
    , height
    , set
    , put
    , get

    , group
    , liberties
    , capture
    , play
    , enclosure
    , partition
    ) where

import Data.Maybe
import Data.List (nubBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Point = Point !Int !Int
           deriving (Eq, Ord)

instance Show Point where
    show (Point x y) = show (x, y)

adjacentPoints :: Point -> [Point]
adjacentPoints (Point x y) = [ Point (x + dx) (y + dy)
                             | dx <- [-1, 0, 1]
                             , dy <- [-1, 0, 1]
                             , (dx == 0) /= (dy == 0)
                             ]

outOfBoundsMessage :: Int -> Int -> Point -> Maybe String
outOfBoundsMessage w h (Point x y)
  | x < 1     = Just $ concat ["x < 1 (", show x, ")"]
  | y < 1     = Just $ concat ["y < 1 (", show y, ")"]
  | x > w     = Just $ concat ["x > w (", show x, " > ", show w, ")"]
  | y > h     = Just $ concat ["y > h (", show y, " > ", show h, ")"]
  | otherwise = Nothing

inBounds :: Int -> Int -> Point -> Bool
inBounds w h p = case outOfBoundsMessage w h p of
    Nothing -> True
    Just _  -> False

inBoard :: Board p -> Point -> Bool
inBoard b = inBounds (width b) (height b)

adjacentBoundedPoints :: Board p -> Point -> [Point]
adjacentBoundedPoints b = filter (inBoard b) . adjacentPoints

data Board p = Board { width    :: !Int
                     , height   :: !Int
                     , contents :: !(Map.Map Point p)
                     }
                     deriving (Show, Functor, Eq, Ord)

-- Modify a 'Board' by just modifying its map.
liftB :: (Map.Map Point p -> Map.Map Point q)
      -> (Board p         -> Board q)
liftB f (Board w h m) = Board w h (f m)

-- Modify a 'Board' by combining two maps of 'Board's.
liftB2 :: (Map.Map Point p -> Map.Map Point q -> Map.Map Point r)
       -> (Board p -> Board q -> Board r)
liftB2 op (Board w h p) (Board _ _ q) = Board w h (p `op` q)

-- | Create an empty board of the given width and height.
-- (Invoking @board w h@ creates a board of width @w@ and height @h@.)
board :: Int -> Int -> Board p
board w h
  | w < 0     = error $ "desired width is negative: " ++ show w
  | h < 0     = error $ "desired height is negative: " ++ show h
  | otherwise = Board w h Map.empty

-- | Get the stone at the given position,
-- or 'Nothing' if there is no stone there
-- or the position is out of bounds.
get :: Point -> Board p -> Maybe p
get p = Map.lookup p . contents

-- | Put a stone from the given player on the board,
-- or remove any stone if the argument is 'Nothing'.
set :: Point -> Maybe p -> Board p -> Board p
set p who = liftB $ Map.alter (const who) p

-- | Put a stone from the given player on the board.
put :: Point -> p -> Board p -> Board p
put p = set p . Just

-- | Capture the group at the given position if it has no liberties.
-- Return @(n, b')@,
-- where @n@ is the number of stones captured
-- (@0@ if the group has liberties, or the size of the group otherwise)
-- and @b'@ is the resulting board
-- (the original board if the group has liberties,
-- or the board without the group otherwise).
capture :: Eq p => Point -> Board p -> (Int, Board p)
capture p b
  | Set.null $ liberties p b =
        let g = group p b
            captureCount = Set.size g
            removeCaptured = flip (Set.foldr Map.delete) g
            b' = liftB removeCaptured b
        in  (captureCount, b')
  | otherwise = (0, b)

-- | Put a stone for the given player at the given position,
-- then capture opponents' stones,
-- then self-capture.
-- Return @(n, b')@,
-- where @n@ is the number of opponents' stones captured
-- and @b'@ is the resulting board.
-- This does not prohibit suicides;
-- the result of a suicide will be @(0, b)@
-- where @b@ is the original board.
play :: Eq p => Point -> p -> Board p -> (Int, Board p)
play pt who b =
    let withPlay = put pt who b
        isOtherPlayer = maybe False (/= who) . (`get` b)
        adj = filter isOtherPlayer $ adjacentBoundedPoints b pt
        adjUniq = nubBy (\p1 p2 -> group p1 b == group p2 b) adj
        (capturedPoints, captures) = unzip $ map (`capture` withPlay) adjUniq
        capturedOpponent = foldl (liftB2 Map.intersection) withPlay captures
        (_, capturedSelf) = capture pt capturedOpponent
    in  (sum capturedPoints, capturedSelf)

-- | Get the positions of all the stones connected to
-- the stone at the given point.
-- The set will be empty if there is no stone at the given point.
group :: Eq p => Point -> Board p -> Set.Set Point
group pt b  = case get pt b of
    Nothing -> Set.empty
    Just owner -> recur owner Set.empty [pt]
  where
    recur _ visited [] = visited
    recur owner visited (pt : pts) =
        let adj = adjacentBoundedPoints b pt
            inGroup x = x `Set.notMember` visited && get x b == Just owner
        in  recur owner (Set.insert pt visited) (pts ++ filter inGroup adj)

-- | Get all the liberties of
-- the group of the stone at the given point.
-- The set will be empty if there is no stone at the given point.
liberties :: Eq p => Point -> Board p -> Set.Set Point
liberties pt b =
    let g = Set.toList $ group pt b
        adj = map (Set.fromList . adjacentBoundedPoints b) g
        candidates = Set.unions adj
    in  Set.filter (isNothing . (`get` b)) candidates

-- | A state type for the reducer action of 'enclosure'.
-- See also 'updateEnclosureState'.
data EnclosureState p = Mixed
                        -- ^ We've found stones of multiple colors.
                        -- No one can own this enclosure.
                      | Sole p
                        -- ^ We've only found stones of this one color.
                        -- Perhaps it is completely surrounding the enclosure.
                      | NoneFound
                        -- ^ We haven't found any stones yet.
                        -- Perhaps we'll eventually give it to one player,
                        -- or perhaps the whole board is empty.

-- | Given the contents of an explored adjacent cell,
-- update the current state of the enclosure search.
updateEnclosureState :: Eq p => EnclosureState p -> Maybe p -> EnclosureState p
updateEnclosureState st Nothing = st
updateEnclosureState Mixed _ = Mixed
updateEnclosureState NoneFound (Just p) = Sole p
updateEnclosureState (Sole p) (Just q)
  | p == q    = Sole p
  | otherwise = Mixed

-- | Flood-fill the board starting from the given point,
-- stopping at points occupied by stones.
-- Return a set of all the points enclosed in the area
-- (which will be the entire board
-- if there is no closed loop surrounding the given point),
-- along with a potential ``owner'' of the enclosure:
-- a player owns the enclosure iff
-- all stones found during the flood fill operation
-- belong to that player,
-- and at least one such stone is found.
--
-- If the given point is not empty,
-- this behaves the same as 'group',
-- and the ``owner'' of the enclosure will be 'Nothing'.
-- Thus, it is safe to treat the return value as a tuple containing
-- a player who should receive points for the enclosure (or no one)
-- and the points in the enclosure itself;
-- if the given point is not empty, there are no points to award.
--
-- It is also thus an invariant
-- that the returned set will always be non-empty
-- and contain at least the given point.
enclosure :: Eq p => Point -> Board p -> (Maybe p, Set.Set Point)
enclosure pt b = case (get pt b, recur NoneFound Set.empty [pt]) of
    (Nothing, (Sole p, pts)) -> (Just p,  pts)
    (Nothing, (_, pts))      -> (Nothing, pts)
    (Just _, _)              -> (Nothing, group pt b)
  where
    recur state visited [] = (state, visited)
    recur state visited (pt : pts) =
        let adj = filter (inBoard b) $ adjacentPoints pt
            state' = foldl updateEnclosureState state $ map (`get` b) adj
            okay x = x `Set.notMember` visited && isNothing (get x b)
            new = filter okay adj
            visited' = Set.unions [Set.singleton pt, Set.fromList new, visited]
            pts' = pts ++ new
        in  recur state' visited' pts'

-- | Partition the board into the regions owned by the different players.
-- That is, if @m@ is the result of @partition b@ and @p@ is a player,
-- then @Map.findWithDefault Set.empty p m@
-- evaluates to the set of points owned by @p@ on board @b@.
partition :: Show p => Ord p => Board p -> Map.Map p (Set.Set Point)
partition (Board w h b) =
    let pts = Set.fromList [ Point x y | x <- [1..w], y <- [1..h] ]
    in  go Map.empty pts
  where
    go acc unvisited
      | Set.null unvisited = acc
      | otherwise =
            let pt = Set.findMin unvisited
                (maybeOwner, pts) = enclosure pt $ Board w h b
                unvisited' = unvisited Set.\\ pts
                update = return . Set.union pts . fromMaybe Set.empty
                acc' = maybe acc
                             (\owner -> Map.alter update owner acc)
                             maybeOwner
            in  go acc' unvisited'
