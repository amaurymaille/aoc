module Main where

import Data.List
import Data.Maybe
import Text.Read

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

cartesianProduct3 :: [a] -> [b] -> [c] -> [(a, b, c)]
cartesianProduct3 xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

linesToInts :: [String] -> [Int]
linesToInts lines = map read lines

-- Parts 1 and 2: bruteforce through cartesian product
processInts :: [Int] -> (Int, Int)
processInts ints = 
    let product = cartesianProduct ints ints
        sumsProds = map (\(first, second) -> (first + second, first * second)) product
    in
        searchAnswers sumsProds
    where
        searchAnswers :: [(Int, Int)] -> (Int, Int)
        searchAnswers lst = (searchP1 lst, searchP2 lst)

        searchP1 = searchAnswer

        searchP2 lst = 
            let product = cartesianProduct lst ints
                sumsProds = map (\((first, second), third) -> (first + third, second * third)) product
            in
                searchAnswer sumsProds

        -- Use find instead of filter as we don't need to produce the whole map
        searchAnswer :: [(Int, Int)] -> Int
        searchAnswer values = snd $ fromJust $ find (\x -> fst x == 2020) values

-- Incomplete version, lacks the second part of the solution
-- Use partial recursion to compute solution to first problem
processIntsSmart :: [Int] -> (Int, Int)
processIntsSmart ints = (searchP1 ints, searchP2 ints)
    where
        searchP1 :: [Int] -> Int
        searchP1 lst =
            searchP1Rec lst lst

        -- This should return a Maybe, this would avoid having that weird -1
        -- if nothing is found
        searchP1Rec (x:xs) (y:ys) = 
            let res = searchP1RecMaybe (x:xs) (y:ys) in case res of 
                Just x -> x
                _      -> searchP1Rec (x:xs) ys
        searchP1Rec [] [] = -1

        searchP1RecMaybe (x:xs) ys = let y = head ys in if x + y == 2020 then Just $ x * y else searchP1RecMaybe xs ys
        searchP1RecMaybe [] ys = Nothing

        searchP2 l = -1

readInput :: IO [String]
readInput = do
    file <- readFile "1.txt"
    return $ lines file

main :: IO ()
main = do
    readInput >>= (\lines -> return $ linesToInts lines) 
              >>= (\ints -> return $ processInts ints)
              >>= (\pair -> print pair)
