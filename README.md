# Assignment 2-Peg Solitaire

## Introduction

In this assignment, you will implement a solver for the one-dimensional peg solitaire puzzle using functional programming techniques, with a focus on the **zipper** data structure. The zipper allows efficient, constant-time access to a focused element within a data structure while maintaining the context around it.

### The Puzzle

The one-dimensional peg solitaire puzzle consists of a sequence of positions, each either empty or containing a peg. We call this sequence the _game state_. For instance:
```
X X X X . X X X X
```

Here, `X` indicates a peg and `.` indicates an empty position.

### Game Rules

A peg can jump (either to the left or right) over an adjacent peg into an empty position, removing the jumped peg. For example, in the state above, the third peg can jump over the fourth, resulting in:
```
X X . . X X X X X
```

The puzzle is **solved** when only one peg remains. Not all starting configurations have a winning solution. For instance:
```
X . . . . . . . X
```
has no solution since no moves are possible and two pegs remain.

## Assignment

In this assignment, you will:
1. Implement a **zipper data structure** for efficient list navigation
2. Use **catamorphisms** and **anamorphisms** to analyze game states
3. Build a **game tree** to explore all possible move sequences
4. Determine if a puzzle has a winning solution using functional composition techniques

You will use catamorphisms (`foldT`), anamorphisms (`unfoldT`), and hylomorphisms (combining both) to solve this problem elegantly.

For all ten programming exercises, you have to explicitly add a type declaration and add documentation to every function you define. Additionally, you also have to write tests in
test/Spec.hs.

### Split over two weeks
The assignment is split into two parts, mainly so you have clear goals for the first week and can check your progress using Momotor output.

In the first week, you focus on exercises 1-4 and submit these for feedback from Momotor.


In the second week, you finish the complete assignment. You submit your
whole project.


### Bonus
The bonus exercise is more challenging, but mandatory for an Excellent grade.

### Submission Checklist

Before submitting, ensure:
- [ ] Each function has a type signature
- [ ] Each function is documented with Haddock comments. For an excellent grade, documentation should be of good quality.
- [ ] At least two sensible tests exist per function in [`test/Spec.hs`](test/Spec.hs). For an excellent grade, more thorough testing is needed.
- [ ] Names and student numbers are present at the top of [`src/PegSolitaire.hs`](src/PegSolitaire.hs) and [`test/Spec.hs`](test/Spec.hs)
- [ ] `cabal build` succeeds without errors
- [ ] `cabal test` passes all tests
- [ ] (Bonus) Exercise 10 is implemented if aiming for an excellent grade.

### Helpful Resources

- [Haskell Language Reference](https://www.haskell.org/)
- [Data.Tree documentation](https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Tree.html) - for `Tree` type and utilities
- [Data.List documentation](https://hackage.haskell.org/package/base-4.17.2.1/docs/Data-List.html) - especially `unfoldr` and `catMaybes`
- [Data.Maybe documentation](https://hackage.haskell.org/package/base-4.17.2.1/docs/Data-Maybe.html)
- [Hoogle](https://hoogle.haskell.org/)

## Exercises

### Program the core functionality in module `PegSolitaire`

#### Exercise 1: `isWinning`

Define function `isWinning` that determines if a game state is winning. A state is winning if and only if exactly one peg remains.

#### Exercise 2: `foldT`

Define function `foldT`, the catamorphism factory for the `Tree` type. Compare this to `foldr` for lists and `foldTree` (from [`Data.Tree`](https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Tree.html)).

#### Exercise 3: `Zipper`, `toZipper`, `fromZipper`, `tryRight`, `tryLeft`
We can store the normal one-dimensional game state in a list. However, we can only access the first element in a list in constant time. Another way to do this is to introduce a `Zipper` structure, we can do this for most data structures, but in this assignment, we focus on the zipper of a list. In a Zipper, you 'walk' through the data structure present. You store the current value under focus, the remainder of the data structure, and a history of how you walked through the structure. For a list, you go from left to right through the list. So the focus is the current value, the remainder of the data structure is the list of all values to the right of the focus, and the history is all the values left of the focus. When you store the history in reverse order, you can prepend the focus when moving to the right.

For instance the list `[1,2,3,4,5]`, we can have `3` as focus. Then, `[4,5]` is the remainder and `[2,1]` is the history. So when we go one place to the right, we get `4` as new focus, `[5]` as a new remainder and `[3,2,1]` as new history.

Define the data structure `Zipper a = ...`, which stores a list of type `[a]` as a zipper structure. Then, define helper functions `toZipper` and `fromZipper` that turn a non-empty list into a non-empty zipper structure and vice versa. Also, define functions `tryRight` and `tryLeft` that change the focus of a zipper one position to the right or left. The `tryRight` and `tryLeft` functions should return a `Maybe` type. If the zipper cannot go further right or left, it should return `Nothing`.

#### Exercise 4: `generateLinearStates` and `generateStates` via `unfoldr`
Given `n` of type `Int`, define the following functions using [`unfoldr`](https://hackage.haskell.org/package/base-4.17.2.1/docs/Data-List.html#v:unfoldr) (from `Data.List`).

**`generateLinearStates`**: This function should give all possible states with `n-1` positions filled with pegs and one empty position. E.g. `generateLinearStates 2` should give the elements `X .` and `. X`.

**`generateStates`**: This function gives a list of all possible states up to size `n`. E.g. `generateStates 2` should give the elements `.`, `X`, `. .`, `X .`, `. X` and `X X`.

*Hint:* There are several ways to go for `generateStates`; one such way uses tupling.

*Note:* Use these functions in the remainder of the Assignment to have a broad test set.

#### Exercise 5: `makeMoves` via `unfoldr`
Define function `makeMoves`, which, given a Zipper for the current game state, generates a list of all game states that can be reached by making one valid move. The returned game states should be zippers.

Hint: Consider using [`catMaybes`](https://hackage.haskell.org/package/base-4.17.2.1/docs/Data-Maybe.html#v:catMaybes) from `Data.Maybe` to collect valid moves.

#### Exercise 6: `unfoldT`
Define function `unfoldT`, which is the anamorphism factory for type `Tree`, to help create a `Tree` from a seed. Compare this to `unfoldr` (from `Data.List`) and `unfoldTree` (from `Data.Tree`).

Note: The first argument of `unfoldT` should be a function that returns a suitable `Either` type. The second argument should be the seed.

#### Exercise 7: `makeGameTree`
Use the `Tree` data type we defined to explore all states that can be reached from an initial state. The node should store the current state and all the next states it can reach by making a valid move. Use a leaf to store a game state that cannot take any more moves. Define function `makeGameTree` as instance of `unfoldT` to produce the game tree for a given initial state given as `Zipper`. All the states in the tree should be zippers.

#### Exercise 8: `hasSolution` as hylo
Define function `hasSolution` that determines if a starting game state has a solution as a hylomorphism, that is, as a catamorphism (defined via `foldT`) after an anamorphism (defined via `unfoldT`). I.e., fold over the output of an application to `makeGameTree`.

*Note*: Its input types should be regular lists, not zippers.

#### Exercise 9: `allSolutions` as hylo
Define function `allSolutions` that gives all possible winning end states as a hylomorphism.

*Note*: Its input types should be regular lists, not zippers.

#### [Bonus] Exercise 10: `getSolution`, `trySolution`
Define function `getSolution` that gives back a sequence of moves that a player can take to get a solution if a starting game state has a solution, it should return a `Maybe` type, where `Nothing` indicates that there is no solution.

Additionally, define function `trySolution`, that given the outcome of `getSolution` can try the moves and gives the game state after taking a sequence of moves. `trySolution` should return a `Maybe` type, where `Nothing` indicates that a move was not valid.

*Notes*: We do not specify what this sequence of moves should look like. Define a suitable data type yourself that fits here. The input types of these functions should be regular lists, not zippers.
Additionally, the following should be a validly typed expression: `(\ps -> trySolution ps =<< getSolution ps)`.

*Important*: Do not break previously defined functions when working on this exercise.
