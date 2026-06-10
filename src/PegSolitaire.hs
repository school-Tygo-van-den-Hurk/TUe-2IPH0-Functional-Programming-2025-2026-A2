{-# LANGUAGE InstanceSigs #-}
-- |
-- Module      : PegSolitaire
-- Description :
-- Copyright   : Tygo van den Hurk (1705709)
--               Kylian Maas (1712861)
-- Date:       : 2026-06-02
-- License     : None
module PegSolitaire
  ( Peg (..),
    Pegs,
    stringToPegs,
    ----
    isWinning,
    generateStates,
    generateLinearStates,
    Zipper (..),
    fromZipper,
    toZipper,
    tryRight,
    tryLeft,
    ----
    makeMoves,
    foldT,
    unfoldT,
    Tree (..),
    makeGameTree,
    hasSolution,
    allSolutions,
    getSolution,
    trySolution,
  )
where

import Data.List (unfoldr)
import Data.Maybe()

-- ~~~ Exercise 1 ~~~ --

-- | Enum symbolizing a `Peg`. Any spot can be either `Empty` or `Peg` (filled).
data Peg = Empty | Peg deriving (Eq, Ord)

-- | A `List` of `Peg`s.
type Pegs = [Peg]

instance Show Peg where
  show Empty = "."
  show Peg = "X"

  showList :: [Peg] -> ShowS
  showList xs = \s -> foldr (\x -> (' ' :) . shows x . (' ' :)) s xs

-- | Converts a `String` of "X" and "." to `Pegs`.
-- Errors on any character other then "X" or ".".
--
-- === Examples
--
-- >>> stringToPegs "XXX..."
-- X  X  X  .  .  .
stringToPegs ::
  -- | The `String` to construct `Pegs` from.
  String ->
  -- | The constructed `Pegs`.
  Pegs
stringToPegs = map f
  where
    f '.' = Empty
    f 'X' = Peg
    f _ = error "Invalid peg string"

-- |
-- Determines if for a given state the game has been won.
--
-- If you've read this, you've lost [the game](https://en.wikipedia.org/wiki/The_Game_(mind_game)).
--
-- === Examples
--
-- >>> isWinning [ Empty Empty Peg Empty Empty ]
-- True
--
-- >>> isWinning [ Peg Empty Empty Empty Peg ]
-- False
isWinning ::
  -- | The `Pegs` symbolizing the game.
  Pegs ->
  -- | Wether or not the game can still be won.
  Bool
isWinning = (== 1) . length . filter (== Peg)

-- ~~~ Exercise 2 ~~~ --

-- | A `Tree` with an arbitrary of children which can be either `Node`s or `Leaf`s.
-- `Leaf`s cannot have any children. But both `Node`s, as well as `Leaf`s hold data.
data Tree a = Leaf a | Node a [Tree a] deriving (Show, Eq, Ord)

-- | Folds a `Tree` recursively into a singular value.
--
-- === Examples
--
-- >>> foldT id (\(x, y) -> x * sum y) (Node 3 [Leaf 2, Leaf 2])
-- 12
--
-- >>> foldT (\x -> x + 1) (\(x, y) -> x * sum y) (Node 3 [Leaf 2, Leaf 2])
-- 18
foldT ::
  -- | The function to transform `Leaf`s in the tree where "a" is the `Leaf`s value.
  (a -> b) ->
  -- | The function to transform `Node`s in the tree where "a" is the `Node`s value
  -- and "[b]" are the transformed value of the children.
  ((a, [b]) -> b) ->
  -- | The `Tree` to fold.
  Tree a ->
  -- | The final folded value of the `Tree`.
  b
foldT transLeaf transNode tree = case tree of
  Leaf value -> transLeaf value
  Node value leafs -> transNode (value, children leafs)
  where
    children = map $ foldT transLeaf transNode

-- ~~~ Exercise 3 ~~~ --

-- | A data structure that allows traversing a `List` in constant time.
data Zipper a = Zipper
  { -- | Everything to the left of the current element in the original `List`.
    -- order is reversed in respect to the original `List`.
    left :: [a],
    -- | The current element in the original `List`.
    current :: a,
    -- | Everything to the right of the current element in the original `List`.
    right :: [a]
  }
  deriving (Show, Eq, Ord)

-- | Creates a `Zipper` from an `List`.
--
-- == Examples
--
-- >>> toZipper []
-- input list of zipper cannot be empty, there would be no current.
--
-- >>> toZipper [1,2,3]
-- Zipper {left = [], current = 1, right = [2,3]}
toZipper ::
  -- | The `List` to create a `Zipper` from.
  [a] ->
  -- | The `Zipper` created.
  Zipper a
toZipper [] = error "input list of zipper cannot be empty, there would be no current."
toZipper (x : xs) =
  Zipper
    { -- \| The `left` of the `Zipper` will slowly be moved one-by-one into `current` and
      --   then into `right` as you `tryLeft`.
      left = [],
      -- \| The `current` of the `Zipper`. Is swapped with the head of `left` when you `tryLeft`
      --   and swapped with the `head` of `right` if you `tryRight`.
      current = x,
      -- \| The `right` of the `Zipper` will slowly be moved one-by-one into `current` and
      --   then into `left` as you `tryRight`.
      right = xs
    }

-- | Creates a `List` from a `Zipper`.
--
-- === Examples
--
-- >>> fromZipper (Zipper {left = [1], current = 2, right = [3]})
-- [1,2,3]
--
-- >>> toZipper (Zipper {left = [], current = 1, right = []})
-- [1]
fromZipper :: Zipper a -> [a]
fromZipper zipper = reverse $ left zipper ++ (current zipper : right zipper)

-- | Tries to move right on the `Zipper`.
--
-- Moves `current` to the `head` of `left`, and then moves the `head` of `right` to
-- `current`. The returning `Zipper` is `Noting` if there was no `right` to replace the
-- `current` with, and `Just` a `Zipper` if there was `right`.
--
-- === Examples
--
-- >>> tryRight (Zipper { left = [], current = 1, right = []})
-- Nothing
--
-- >>> tryRight (Zipper { left = [], current = 1, right = [2]})
-- Just (Zipper {left = [1], current = 2, right = []})
tryRight ::
  -- | The `Zipper` to move the `current` into `left`.
  Zipper a ->
  -- | The `Zipper` is `Noting` if there was no `right` to replace the `current` with, and `Just`
  -- a `Zipper` where there was `right`.
  Maybe (Zipper a)
tryRight (Zipper _ _ []) = Nothing
tryRight (Zipper ls c (r : rs)) =
  Just
    ( Zipper
        { left = c : ls,
          current = r,
          right = rs
        }
    )

-- | Tries to move left on the `Zipper`.
--
--  Moves `current` to the `head` of `right`, and then moves the `head` of `left` to
-- `current`. The returning `Zipper` is `Noting` if there was no `left` to replace the
-- `current` with, and `Just` a `Zipper` if there was `left`.
--
-- === Examples
--
-- >>> tryLeft (Zipper { left = [], current = 1, right = []})
-- Nothing
--
-- >>> tryLeft (Zipper { left = [1], current = 2, right = []})
-- Just (Zipper {left = [], current = 1, right = [2]})
tryLeft ::
  -- | The `Zipper` to move the `current` into `right`.
  Zipper a ->
  -- | The `Zipper` is `Noting` if there was no `left` to replace the `current` with, and `Just`
  -- a `Zipper` if there was `left`.
  Maybe (Zipper a)
tryLeft (Zipper [] _ _) = Nothing
tryLeft (Zipper (l : ls) c rs) =
  Just
    ( Zipper
        { left = ls,
          current = l,
          right = c : rs
        }
    )

-- ~~~ Exercise 4 ~~~ --

-- | Generates all combinations of `Pegs` of length of a given `Integer`.
--
-- === Examples
--
-- >>> (map (map show) . generateStates) 0
--
-- >>> (map (map show) . generateStates) 1
--
-- >>> (map (map show) . generateStates) 2
generateStates ::
  -- | The size of the output `Pegs`.
  Integer ->
  -- | All possible permutations of `Pegs` of size n.
  [Pegs]
generateStates n = unfoldr step (0, [])
  where
    step (i, _) | i > 2 * n = Nothing
    step (i, arr) = Just ([Empty], (i + 1, arr))

    -- | Generates a `Tree` of depth n where all nodes hold a `Bool`.
    genT :: Integer -> Integer -> [Tree Bool]
    genT i t | i + 1 > t = error "This is not a cas we consider"
    genT i t | i + 1 == t = [Leaf True, Leaf False]
    genT i t = let subtree = genT (i + 1) t
      in [ Node True subtree, Node False subtree ]

    -- | Flattens the `Tree` into a `List` of `List`s of `Bool`s.
    flattenT :: Tree Bool -> [[Bool]]
    flattenT = foldT foldLeaf foldNode
      where
        foldLeaf value = [[value]]
        foldNode (value, children) = concatMap (map (value :)) children

-- | Generates all states with exactly one Empty and the rest Peg of size n.
--
-- === Examples
--
-- >>> (map (map show) . generateLinearStates) 3
-- [[".","X","X"],["X",".","X"],["X","X","."]]
--
-- >>> (map (map show) . generateLinearStates) 4
-- [[".","X","X","X"],["X",".","X","X"],["X","X",".","X"],["X","X","X","."]]
generateLinearStates ::
  -- | The size of `Pegs`
  Int ->
  -- | All combinations of size n where one `Peg` is `Empty`.
  [Pegs]
generateLinearStates n = unfoldr step 0
  where
    comb i = [if j == i then Empty else Peg | j <- [0 .. n - 1]]
    step i | i >= n = Nothing
    step i = Just (comb i, i + 1)

-- ~~~ Exercise 5 ~~~ --

-- | A game state `Pegs` represented as a `Zipper` to analyze quicker.
type State = Zipper Peg

-- | A list of game `State`s.
type States = [State]

-- | The 2 directions of a 1D line.
data Direction = 
  -- | Left of something.
  L | 
  -- | Right of something.
  R

-- | Returns all possible legal `State`s the game can go in for a given `State`.
makeMoves :: 
  -- | The `State` of the game to move from.
  State ->
  -- | All possible legal `State`s from there.
  States
makeMoves zipper = unfoldr rec (R, Just zipper, Just zipper)
  where
    -- | A recursion on a `State`.
    rec :: (
        -- | The `Direction` to move the `Peg` in.
        Direction, 
        -- | A `State` that might have reached the end of the list.
        Maybe State,
        -- | A `State` that might have reached the end of the list.
        Maybe State
      ) -> Maybe (State, (Direction, Maybe State, Maybe State))
    
    -- We cannot move in either direction: we've reached the ends.
    rec (_, Nothing, Nothing) = Nothing

    -- current pin is empty: we cannot move these
    rec (_, Just l@Zipper { current = Empty }, r) = rec (R, tryLeft l, r)
    rec (_, l, Just r@Zipper { current = Empty }) = rec (R, l, tryRight r)
    
    -- We want to move a pin, but there is not enough space. 
    -- We first try looking left still, if we can't do either we move on.
    rec (R, Just l@Zipper { right = [] }, r) = rec (L, Just l, r)
    rec (R, Just l@Zipper { right = [_] }, r) = rec (L, Just l, r)
    rec (R, l, Just r@Zipper { right = [] }) = rec (L, l, Just r)
    rec (R, l, Just r@Zipper { right = [_] }) = rec (L, l, Just r)
    rec (L, Just l@Zipper { left = [] }, r) = rec (R, tryLeft l, r)
    rec (L, Just l@Zipper { left = [_] }, r) = rec (R, tryLeft l, r)
    rec (L, l, Just r@Zipper { left = [] }) = rec (R, l, tryRight r)
    rec (L, l, Just r@Zipper { left = [_] }) = rec (R, l, tryRight r)

    -- Our next neighbor is a `Peg`, and the one next to that is `Empty` thus a valid move. 
    -- Keep in mind the direction we're checking.
    rec (R, Just l@Zipper { 
      right = (Peg:Empty:rest)
    }, r) = Just (Zipper { 
      left = left l,
      current = Empty,
      right = Empty:Peg:rest
    }, (L, Just l, r) )

    rec (R, Just l@Zipper { 
      left = (Peg:Empty:rest)
    }, r) = Just (Zipper { 
      left = Empty:Peg:rest,
      current = Empty,
      right = right l
    }, (R, tryLeft l, r) )

    rec (R, l, Just r@Zipper { 
      right = (Peg:Empty:rest)
    }) = Just (Zipper { 
      left = left r,
      current = Empty,
      right = Empty:Peg:rest
    }, (L, l, Just r) )

    rec (L, l, Just r@Zipper { 
      left = (Peg:Empty:rest)
    }) = Just (Zipper { 
      left = Empty:Peg:rest,
      current = Empty,
      right = right r
    }, (R, l, tryRight r) )

    -- stop the warnings pls
    -- rec _ = Nothing

-- error "Implement, document, and test this function"

-- ~~~ Exercise 6 ~~~ --

-- | Creates a `Tree` from a seed.
-- 
-- Must return `Either` a value, or a value and a `List` of seeds. Those will become `Leaf`s and
-- `Node`s respectively.
--
-- === Examples
--
-- >>> unfoldT (\x -> if x <= 0 then Left 0 else Right (x, [x-1,x-2])) 3
-- Node 3 [Node 2 [Node 1 [Leaf 0,Leaf 0],Leaf 0],Node 1 [Leaf 0,Leaf 0]]
-- 
unfoldT ::
  -- | The function to create trees from
  (b -> Either a (a, [b])) ->
  -- | The seed to create the tree from.
  b ->
  -- | The resulting `Tree` from that seed.
  Tree a
unfoldT generator seed = case generator seed of
  Left value -> Leaf value
  Right (value, seeds) -> Node value children
    where
      children = map (unfoldT generator) seeds

-- Exercise 7 --

makeGameTree = error "Implement, document, and test this function"

hasSolution = error "Implement, document, and test this function"

allSolutions = error "Implement, document, and test this function"

getSolution = error "Implement, document, and test this function"

trySolution = error "Implement, document, and test this function"
