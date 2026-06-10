-- \|
-- Module      : PegSolitaire
-- Description :
-- Copyright   : Tygo van den Hurk (1705709)
--               Kylian Maas (1712861)
-- Date:       : 2026-06-02
-- License     : None
import Control.Exception (evaluate)
import PegSolitaire
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  -- ~~~ Exercise 1 ~~~ --

  describe "isWinning" $ do
    let o = Empty :: Peg
    let x = Peg :: Peg

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
        let sumT = foldT id (\(x, y) -> x + sum y)
        sumT tree `shouldBe` (7 :: Integer)
      it "elemT" $ do
        let elemT x = foldT (== x) (\(y, z) -> (y == x) || or z)
        (1 `elemT` tree) `shouldBe` True
        (2 `elemT` tree) `shouldBe` True
        (3 `elemT` tree) `shouldBe` False
        (4 `elemT` tree) `shouldBe` True
        (5 `elemT` tree) `shouldBe` False
      it "mapT" $ do
        let mapT f = foldT (Leaf . f) (\(x, y) -> Node (f x) y)
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
        tryRight Zipper {
          left = ['b', 'c', 'd', 'e'],
          current = 'a',
          right = []
        } `shouldBe` Nothing

  -- ~~~ Exercise 4 ~~~ --

  describe "generateStates" $ do
    it "generateStates 0 == []" $ do
      generateStates 0 `shouldBe` [[]]
    it "generateStates 1 is a set of [Empty] and [Peg]" $ do
      let result = generateStates 1
      length result `shouldBe` 2
      result `shouldSatisfy` elem [Peg]
      result `shouldSatisfy` elem [Empty]
    it "generateStates 2 contains all 4 combinations" $ do
      let result = generateStates 2
      length result `shouldBe` 4
      result `shouldSatisfy` elem [Empty, Empty]
      result `shouldSatisfy` elem [Empty, Peg]
      result `shouldSatisfy` elem [Peg, Empty]
      result `shouldSatisfy` elem [Peg, Peg]
    it "generateStates 3 contains all 8 combinations" $ do
      let result = generateStates 3
      length result `shouldBe` 8
      result `shouldSatisfy` elem [Empty, Empty, Empty]
      result `shouldSatisfy` elem [Empty, Empty, Peg]
      result `shouldSatisfy` elem [Empty, Peg, Empty]
      result `shouldSatisfy` elem [Empty, Peg, Peg]
      result `shouldSatisfy` elem [Peg, Empty, Empty]
      result `shouldSatisfy` elem [Peg, Empty, Peg]
      result `shouldSatisfy` elem [Peg, Peg, Empty]
      result `shouldSatisfy` elem [Peg, Peg, Peg]
    it "generateStates 3 contains all 8 combinations" $ do
      let result = generateStates 3
      length result `shouldBe` 8
      result `shouldSatisfy` elem [Empty, Empty, Empty]
      result `shouldSatisfy` elem [Empty, Empty, Peg]
      result `shouldSatisfy` elem [Empty, Peg, Empty]
      result `shouldSatisfy` elem [Empty, Peg, Peg]
      result `shouldSatisfy` elem [Peg, Empty, Empty]
      result `shouldSatisfy` elem [Peg, Empty, Peg]
      result `shouldSatisfy` elem [Peg, Peg, Empty]
      result `shouldSatisfy` elem [Peg, Peg, Peg]

  describe "generateLinearStates" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)

  -- ~~~ Exercise 5 ~~~ --

  describe "makeMoves" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "unfoldT" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "makeGameTree" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "hasSolution" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "allSolutions" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "getSolution" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "trySolution" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)
