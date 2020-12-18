module Main where

import Data.List.Split
import Debug.Trace

processInput :: [String] -> Int
processInput lines = let groups = map merge $ splitOnEmptyLine lines in processGroups groups
    where
        merge :: [String] -> [String]
        merge lst = foldl (++) [] $ map words lst

        splitOnEmptyLine :: [String] -> [[String]]
        splitOnEmptyLine lst = walk lst []
            where
                walk :: [String] -> [String] -> [[String]]
                walk [] result = [result]
                walk (x:xs) res = 
                    if x == "" then 
                        let tail = walk xs [] in res:tail
                    else
                        walk xs (res ++ [x])

        processGroups :: [[String]] -> Int
        processGroups groups = sum (map processGroup groups)

        processGroup :: [String] -> Int
        processGroup group =
            let n = length group in
                if n == 8 then 
                    1
                else if n == 7 then
                    isCidMissing group
                else
                    0
            where 
                isCidMissing :: [String] -> Int
                isCidMissing group = if any isCid group then 0 else 1

                isCid :: String -> Bool
                isCid str = head (splitOn ":" str) == "cid"

main :: IO ()
main = readFile "4.txt" >>= \content -> print (processInput $ lines content)
