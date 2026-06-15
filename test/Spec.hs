-- Module      : PegSolitaire
-- Description :
-- Copyright   : Tygo van den Hurk (1705709)
-- Date:       : 2026-06-02
-- License     : None
import Control.Exception (evaluate)
import PegSolitaire
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  let o = Empty :: Peg
  let x = Peg :: Peg

  -- ~~~ Exercise 1 ~~~ --

  describe "isWinning" $ do
    it "X.X.X.X : not Win" $ do
      isWinning [x, o, x, o, x, o, x] `shouldBe` False
    it "X...... : Win" $ do
      isWinning [x, o, o, o, o, o, o] `shouldBe` True
    it "X.....X : not Win" $ do
      isWinning [x, o, o, o, o, o, x] `shouldBe` False
    it "....X.. : Win" $ do
      isWinning [o, o, o, o, x, o, o] `shouldBe` True

  -- ~~~ Exercise 2 ~~~ --

  describe "Tree a" $ do
    let tree = Node 4 [Leaf 1, Leaf 2]

    describe "foldT" $ do
      it "sumT" $ do
        let sumT = foldT id (\(a, b) -> a + sum b)
        sumT tree `shouldBe` (7 :: Integer)
      it "elemT" $ do
        let elemT a = foldT (== a) (\(b, c) -> (b == a) || or c)
        (1 `elemT` tree) `shouldBe` True
        (2 `elemT` tree) `shouldBe` True
        (3 `elemT` tree) `shouldBe` False
        (4 `elemT` tree) `shouldBe` True
        (5 `elemT` tree) `shouldBe` False
      it "mapT" $ do
        let mapT f = foldT (Leaf . f) (\(y, z) -> Node (f y) z)
        mapT (+ 1) tree `shouldBe` Node 5 [Leaf 2, Leaf 3]
        mapT even tree `shouldBe` Node True [Leaf False, Leaf True]

  -- ~~~ Exercise 3 ~~~ --

  describe "Zipper" $ do
    let list = ['a', 'b', 'c', 'd', 'e']
    let zipper = toZipper list
    let midZipper =
          Zipper
            { left = ['b', 'a'],
              current = 'c',
              right = ['d', 'e']
            }

    describe "fromZipper and toZipper" $ do
      it "toZipper expectation" $ do
        zipper `shouldBe` toZipper list
        zipper
          `shouldBe` Zipper
            { left = [],
              current = 'a',
              right = ['b', 'c', 'd', 'e']
            }
      it "fromZipper . toZipper == id_list" $ do
        (fromZipper . toZipper) list `shouldBe` list
      it "fromZipper preserves list order" $ do
        fromZipper zipper `shouldBe` list
        fromZipper midZipper `shouldBe` list

    describe "`tryRight` and `tryLeft`" $ do
      it "tryRight (tryRight zipper) == midZipper" $ do
        case tryRight zipper of
          Nothing -> expectationFailure "Should not have returned Nothing"
          Just moved -> case tryRight moved of
            Nothing -> expectationFailure "Should not have returned Nothing"
            Just result -> result `shouldBe` midZipper
      it "tryLeft (tryLeft midZipper) == zipper" $ do
        case tryLeft midZipper of
          Nothing -> expectationFailure "Should not have returned Nothing"
          Just moved -> case tryLeft moved of
            Nothing -> expectationFailure "Should not have returned Nothing"
            Just result -> result `shouldBe` zipper
      it "tryLeft zipper" $ do
        case tryLeft midZipper of
          Nothing -> expectationFailure "Should not have returned Nothing"
          Just result ->
            result
              `shouldBe` Zipper
                { left = ['a'],
                  current = 'b',
                  right = ['c', 'd', 'e']
                }
      it "tryRight zipper" $ do
        case tryRight midZipper of
          Nothing -> expectationFailure "Should not have returned Nothing"
          Just result ->
            result
              `shouldBe` Zipper
                { left = ['c', 'b', 'a'],
                  current = 'd',
                  right = ['e']
                }
      it "`tryRight` after `tryLeft` is `id` for midZipper" $ do
        case tryLeft midZipper of
          Nothing -> expectationFailure "Should not have returned Nothing (tryLeft)"
          Just movedZipper -> case tryRight movedZipper of
            Nothing -> expectationFailure "Should not have returned Nothing (tryRight)"
            Just result -> result `shouldBe` midZipper
      it "`tryLeft` after `tryRight` is `id` for midZipper" $ do
        case tryRight midZipper of
          Nothing -> expectationFailure "Should not have returned Nothing (tryRight)"
          Just movedZipper -> case tryLeft movedZipper of
            Nothing -> expectationFailure "Should not have returned Nothing (tryLeft)"
            Just result -> result `shouldBe` midZipper
      it "`tryLeft` on `zipper` is `Nothing`" $ do
        tryLeft zipper `shouldBe` Nothing
      it "`tryRight` on `rightZipper` is `Nothing`" $ do
        tryRight
          Zipper
            { left = ['b', 'c', 'd', 'e'],
              current = 'a',
              right = []
            }
          `shouldBe` Nothing

  -- ~~~ Exercise 4 ~~~ --

  describe "generateStates" $ do
    it "generateStates 0 == []" $ do
      generateStates 0 `shouldBe` []
    it "generateStates 1 is a set of [Empty] and [Peg]" $ do
      let result = generateStates 1
      result `shouldSatisfy` elem [Peg]
      result `shouldSatisfy` elem [Empty]
      length result `shouldBe` 2
    it "generateStates 2 contains all 6 combinations" $ do
      let result = generateStates 2
      result `shouldSatisfy` elem [Empty]
      result `shouldSatisfy` elem [Peg]
      result `shouldSatisfy` elem [Empty, Empty]
      result `shouldSatisfy` elem [Empty, Peg]
      result `shouldSatisfy` elem [Peg, Empty]
      result `shouldSatisfy` elem [Peg, Peg]
      length result `shouldBe` 6
    it "generateStates 3 contains all 14 combinations" $ do
      let result = generateStates 3
      result `shouldSatisfy` elem [Empty]
      result `shouldSatisfy` elem [Peg]
      result `shouldSatisfy` elem [Empty, Empty]
      result `shouldSatisfy` elem [Empty, Peg]
      result `shouldSatisfy` elem [Peg, Empty]
      result `shouldSatisfy` elem [Peg, Peg]
      result `shouldSatisfy` elem [Empty, Empty, Empty]
      result `shouldSatisfy` elem [Empty, Empty, Peg]
      result `shouldSatisfy` elem [Empty, Peg, Empty]
      result `shouldSatisfy` elem [Empty, Peg, Peg]
      result `shouldSatisfy` elem [Peg, Empty, Empty]
      result `shouldSatisfy` elem [Peg, Empty, Peg]
      result `shouldSatisfy` elem [Peg, Peg, Empty]
      result `shouldSatisfy` elem [Peg, Peg, Peg]
      length result `shouldBe` 14

  describe "generateLinearStates" $ do
    it "generateLinearStates 3 produces all single-empty states of length 3" $ do
      map (concatMap show) (generateLinearStates 3) `shouldBe` [".XX", "X.X", "XX."]

    it "generateLinearStates 4 produces 4 states each with exactly one Empty" $ do
      let states = generateLinearStates 4
      length states `shouldBe` 4
      all (\ps -> length (filter (== Empty) ps) == 1 && length ps == 4) states `shouldBe` True

  -- ~~~ Exercise 5 ~~~ --

  describe "makeMoves" $ do
    it "makeMoves on 'XX.' gives one move resulting in '..X'" $ do
      map fromZipper (makeMoves (toZipper (stringToPegs "XX."))) `shouldBe` [stringToPegs "..X"]
    it "makeMoves on 'X.X' gives no moves" $ do
      makeMoves (toZipper (stringToPegs "X.X")) `shouldBe` []

  -- ~~~ Exercise 6 ~~~ --

  describe "unfoldT" $ do
    it "unfoldT builds the expected tree from seed 3" $ do
      let rec a = if a <= 0 then Left 0 else Right (a, [a - 1, a - 2])
      unfoldT rec (3 :: Integer) `shouldBe` Node 3 [Node 2 [Node 1 [Leaf 0, Leaf 0], Leaf 0], Node 1 [Leaf 0, Leaf 0]]
    it "unfoldT on a Left seed produces a single Leaf" $ do
      let rec a = if a <= 0 then Left a else Right (a, [a - 1])
      unfoldT rec (0 :: Integer) `shouldBe` Leaf 0

  -- ~~~ Exercise 7 ~~~ --

  describe "makeGameTree" $ do
    let mapT f = foldT (Leaf . f) (\(y, z) -> Node (f y) z)
    let fromZipperT = mapT fromZipper
    it "Game Tree of 'X.X'" $ do
      let game = toZipper $ stringToPegs "X.X"
      let tree = makeGameTree game
      fromZipperT tree `shouldBe` Leaf [x, o, x]
    it "Game Tree of 'X.XX'" $ do
      let game = toZipper $ stringToPegs "X.XX"
      let tree = makeGameTree game
      fromZipperT tree `shouldBe` Node [x, o, x, x] [Node [x, x, o, o] [Leaf [o, o, x, o]]]
    it "Game Tree of '.X.XX'" $ do
      let game = toZipper $ stringToPegs ".X.XX"
      let tree = makeGameTree game
      fromZipperT tree `shouldBe` Node [o, x, o, x, x] [Node [o, x, x, o, o] [Leaf [o, o, o, x, o], Leaf [x, o, o, o, o]]]

  -- ~~~ Exercise 8 ~~~ --

  describe "hasSolution" $ do
    it "No solutions for 'X.X'" $ do
      let game = stringToPegs "X.X"
      hasSolution game `shouldBe` False
    it "One Solution for 'XX.'" $ do
      let game = stringToPegs "XX."
      hasSolution game `shouldBe` True
    it "One Solution for 'X.XX'" $ do
      let game = stringToPegs "X.XX"
      hasSolution game `shouldBe` True
    it "No Solutions for 'X..XX'" $ do
      let game = stringToPegs "X..XX"
      hasSolution game `shouldBe` False

  -- ~~~ Exercise 9 ~~~ --

  describe "allSolutions" $ do
    it "No solutions for 'X.X'" $ do
      let game = stringToPegs "X.X"
      let none = []
      allSolutions game `shouldBe` none
    it "One Solution for '.XX'" $ do
      let game = stringToPegs ".XX"
      let solution = [x, o, o]
      allSolutions game `shouldBe` [solution]
    describe "Two Solutions for 'XX.X.'" $ do
      let game = stringToPegs "XX.X."
      let solutions = allSolutions game
      it "solution 1: '....X'" $ do
        let solution1 = [o, o, o, o, x]
        solutions `shouldSatisfy` elem solution1
      it "solution 2: '.X...'" $ do
        let solution2 = [o, x, o, o, o]
        solutions `shouldSatisfy` elem solution2

  -- ~~~ Bonus Exercise ~~~ --

  describe "getSolution" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "trySolution" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)
