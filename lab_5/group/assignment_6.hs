-- Assignment: Lab5
-- Exercise: 6
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 90 minutes
--------------------------------------------------------------------------

module Assignment_6 where
 
import Data.List
import System.Random
import Lecture5

--------------------------------------------------------------------------
-- Q1. Can you find a way of classifying the difficulty of a Sudoku problem?
--------------------------------------------------------------------------
-- Pelanek outlines two main ways that he used to classify the difficulty of a sudoku puzzle.
-- The first one is modeling human sudoku-solving techniques in the solver.
-- In order to calculate the difficulty of the sudoku he uses two metrics.
-- Those metrics are refutation score and step dependency.
-- The refutation score is the number of steps that need to be taken in order to
-- prove that putting a value in the specific cell makes it inconsistent.
-- The lower the refutation score for a cell, the easier it is to find a suitable value 
-- for it. The higher the number of low refutation score cells, the easier the sudoku.
-- The step dependency metric is the average number of possible next steps.
-- The second technique is called constraint relaxation.
-- The constraints on a sudoku problem are being removed and the algorithm checks
-- how many solutions does a sudoku have after removing the said constraints.
-- In case of the harder sudokus, the number of solutions when subsequent constraints are removed
-- grows faster than in case of easy sudokus, because the easy sudokus
-- have more ways to reach a solution.
-- A different way of evaluating the difficulty that can be applied to backtracking solvers
-- is the number of backtracking steps.
--------------------------------------------------------------------------
-- Q2. Can you modify the Sudoku problem generator so that it can generate problems that are minimal, but easy to solve by hand?
--------------------------------------------------------------------------
-- Problems that are minimal and hard to solve by hand tend to have more possibilities of finding next steps.
-- Humans prefer simple logic techniques (Pelanek, 3.1), which is why the sudokus with
-- a greater number of possible steps that can be made to progress in the puzzle
-- are easier to solve for humans.
--------------------------------------------------------------------------
-- Q3. Problems that are minimal, but hard to solve by hand?
--------------------------------------------------------------------------
-- Problems that are minimal and hard to solve by hand tend to have less possibilities. Since humans (Pelanek, 3.1)
-- are not very good at performing systematic searches, it is hard to find the valid steps.
--------------------------------------------------------------------------
-- Q4. How can you test whether the problems your program generates satisfy these properties? Consult (Pelánek 2014).
--------------------------------------------------------------------------
-- I could use a number of properties related to the classification from answer to question 1.
-- The easiest thing to check is the step dependency. According to Pelanek (Table 2), the 
-- dependency metric is higher for the easy sudokus (> 0.7) and lower for the whole dataset (< 0.7), which means 
-- that the hard sudokus have a lower dependency metric value. 
--------------------------------------------------------------------------

