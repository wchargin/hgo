module TestUtil (
      boardFromAscii
    , XO(..)
    , xo
    , ABCD(..)
    , abcd
    ) where

import GoBoard
import Data.List (nub)

{-# ANN module "HLint: ignore Use String" #-}

boardFromAscii :: (Char -> Maybe p) -> [[Char]] -> Board p
boardFromAscii f css =
    let h = length css
        w = case nub $ map length css of
            [] -> 0
            [w'] -> w'
            ws -> error $ "non-unique widths: " ++ show ws
        updateBoard b a =
            foldl (\acc (y, r) -> updateRow y acc r) b (zip [1..] a)
        updateRow y b r =
            foldl (\acc (x, c) -> updateCell x y acc c) b (zip [1..] r)
        updateCell x y b c = set (Point x y) (f c) b
    in  updateBoard (board w h) (reverse css)  -- row 1 at bottom

data XO = X | O deriving (Eq, Ord, Show)

xo :: Char -> Maybe XO
xo = flip lookup [ ('x', X), ('X', X)
                 , ('o', O), ('O', O)
                 ]

data ABCD = A | B | C | D deriving (Eq, Ord, Show)

abcd :: Char -> Maybe ABCD
abcd = flip lookup [ ('a', A), ('A', A)
                   , ('b', B), ('B', B)
                   , ('c', C), ('C', C)
                   , ('d', D), ('D', D)
                   ]
