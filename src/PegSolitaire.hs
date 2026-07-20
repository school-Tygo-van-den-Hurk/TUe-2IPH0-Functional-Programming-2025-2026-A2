{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      : PegSolitaire
-- Description :
-- Copyright   : Tygo van den Hurk (1705709)
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
import Data.Maybe
  ( catMaybes,
  )

-- ~~~ Exercise 1 ~~~ --

-- | Enum symbolizing a `Peg`. Any spot can be either `Empty` or `Peg` (filled).
data Peg
  = -- | An empty spot with no `Peg` in it.
    Empty
  | -- | An occupied spot with a `Peg` in it.
    Peg
  deriving (Eq, Ord)

-- | A `List` of `Peg`s.
type Pegs = [Peg]

instance Show Peg where
  show Empty = "."
  show Peg = "X"

  showList :: [Peg] -> ShowS
  showList xs s = foldr (\x -> (' ' :) . shows x . (' ' :)) s xs

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
stringToPegs = map toPeg
  where
    toPeg '.' = Empty
    toPeg 'X' = Peg
    toPeg _ = error "Invalid peg string"

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
data Tree a
  = -- | The end of a tree. Any Vertex without any children.
    Leaf
      -- | The value in the `Leaf`.
      a
  | -- | Any Vertex without any children.
    Node
      -- | The value in the `Node`.
      a
      -- | The children of the `Node`.
      [Tree a]
  deriving (Show, Eq, Ord)

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
  (a -> [b] -> b) ->
  -- | The `Tree` to fold.
  Tree a ->
  -- | The final folded value of the `Tree`.
  b
foldT transLeaf transNode tree = case tree of
  Leaf value -> transLeaf value
  Node value leafs -> transNode value $ children leafs
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
fromZipper zipper = reverse (left zipper) ++ (current zipper : right zipper)

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

-- | Generates all combinations of `Pegs` of length of a given `Int`.
--
-- === Examples
--
-- >>> map (concatMap show) $ generateStates 0
-- []
--
-- >>> map (concatMap show) $ generateStates 1
-- [".","X"]
--
-- >>> map (concatMap show) $ generateStates 2
-- [".","X","..",".X","X.","XX"]
generateStates ::
  -- | The size of the output `Pegs`.
  Int ->
  -- | All possible permutations of `Pegs` of size n.
  [Pegs]
generateStates n | n <= 0 = []
generateStates n = concat $ unfoldr step 1
  where
    step i | i > n = Nothing
    step i = Just (combos i, i + 1)
    combos 0 = [[]]
    combos k = [p : ps | p <- [Empty, Peg], ps <- combos (k - 1)]

-- | Generates all states with exactly one Empty and the rest Peg of size n.
--
-- === Examples
--
-- >>> map (concatMap show) $ generateLinearStates 3
-- [".XX","X.X","XX."]
--
-- >>> map (concatMap show) $ generateLinearStates 4
-- [".XXX","X.XX","XX.X","XXX."]
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

-- | Returns all possible legal `State`s the game can go in for a given `State`.
--
-- === Examples
--
-- >>> length $ makeMoves $ toZipper (stringToPegs "XX.")
-- 1
--
-- >>> length $ makeMoves $ toZipper (stringToPegs "X.X")
-- 0
makeMoves ::
  -- | The `State` of the game to move from.
  Zipper Peg ->
  -- | All possible legal `State`s from there.
  [Zipper Peg]
makeMoves zipper = unfoldr step (positions zipper)
  where
    -- \| All zipper positions reachable by repeatedly moving right from the
    -- leftmost position.
    positions :: Zipper Peg -> [Zipper Peg]
    positions z = goLeftmost z : unfoldr (\zz -> tryRight zz >>= \z' -> Just (z', z')) (goLeftmost z)

    goLeftmost :: Zipper Peg -> Zipper Peg
    goLeftmost z = maybe z goLeftmost (tryLeft z)

    -- \| For each position, try a jump to the right (peg, peg, empty -> empty, empty, peg)
    -- and a jump to the left (empty, peg, peg -> peg, empty, empty), collecting valid results.
    step :: [Zipper Peg] -> Maybe (Zipper Peg, [Zipper Peg])
    step [] = Nothing
    step (z : zs) =
      case catMaybes [jumpRight z, jumpLeft z] of
        (m : _) -> Just (m, zs)
        [] -> step zs

    -- \| Jump the current peg over its right neighbor into an empty spot two to the right.
    jumpRight :: Zipper Peg -> Maybe (Zipper Peg)
    jumpRight z@Zipper {current = Peg, right = (Peg : Empty : rs)} =
      Just z {current = Empty, right = Empty : Peg : rs}
    jumpRight _ = Nothing

    -- \| Jump the current peg over its left neighbor into an empty spot two to the left.
    jumpLeft :: Zipper Peg -> Maybe (Zipper Peg)
    jumpLeft z@Zipper {current = Peg, left = (Peg : Empty : ls)} =
      Just z {current = Empty, left = Empty : Peg : ls}
    jumpLeft _ = Nothing

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

-- | Generates all possible moves from a state of `Pegs`, represented as a `Zipper`
-- of `Peg`.
--
-- === Examples
--
-- >>> makeGameTree ".XX"
--
-- >>> map (concatMap show) $ allSolutions $ makeGameTree "XX.XX"
makeGameTree ::
  -- | The game to get all the states from.
  Zipper Peg ->
  -- | All possible legal game states you can reach from there.
  Tree (Zipper Peg)
makeGameTree = unfoldT toGameState
  where
    toGameState zipper = case makeMoves zipper of
      [] -> Left zipper
      list -> Right (zipper, list)

-- ~~~ Exercise 8 ~~~ --

-- | Given a `List` of `Pegs`, Sees if there is a order of moves you
-- can perform to win the game. Returns `True` if there is at least one
-- such set of moves to win the game.
--
-- === Examples
hasSolution ::
  -- | The game of `Pegs` to analyze.
  Pegs ->
  -- | Whether it has a solution.
  Bool
hasSolution game = foldT leafCase caseNode $ makeGameTree $ toZipper game
  where
    caseNode vertex subTrees = leafCase vertex || or subTrees
    leafCase = isWinning . fromZipper

-- ~~~ Exercise 9 ~~~ --

-- | Generates all solutions to a given game of `Pegs`.
--
-- === Examples
--
-- >>> map (concatMap show) $ allSolutions $ stringToPegs "X.X"
-- []
--
-- >>> map (concatMap show) $ allSolutions $ stringToPegs ".XX"
-- [".X."]
--
-- >>> map (concatMap show) $ allSolutions $ stringToPegs "XX.X."
-- ["X....","...X."]
allSolutions ::
  -- | The game of `Pegs` to analyze.
  Pegs ->
  -- | All solutions to the given game from that point on.
  [Pegs]
allSolutions game = foldT leafCase caseNode $ makeGameTree $ toZipper game
  where
    -- We only need to check the `Leaf`s, as a winning game cannot make more moves.
    leafCase zipper = [fromZipper zipper | isWinning $ fromZipper zipper]
    caseNode _ = concat

-- ~~~ Bonus Exercise ~~~ --

-- | Given a game state, returns a list of `Pegs` representing the steps you must take
-- to end up in a winning configuration. Returns `Nothing` if no such move exists.
--
-- === Examples
getSolution = error "Implement, document, and test this function"

trySolution = error "Implement, document, and test this function"
