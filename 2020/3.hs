module Main where

getInput :: IO [String]
getInput = readFile "3.txt" >>= \content -> return $ lines content

printInput :: [String] -> IO ()
printInput (x:xs) = print x >> 
                    printInput xs
printInput [] = return ()

-- Keep all N element of a list (starting with the first)
allN :: [a] -> Int -> [a]
allN lst n = allNRec lst n 0
    where
        allNRec :: [a] -> Int -> Int -> [a]
        allNRec (x:xs) n depth = 
            if depth == 0 then
                let res = allNRec xs n (n - 1) in
                (x:res)
            else
                allNRec xs n (depth - 1)
        allNRec [] _ _ = []

processLines :: [String] -> Int
processLines lines = product (map (\(right, down) -> processOneSlope lines right down) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)])
    where 
        processOneSlope :: [String] -> Int -> Int -> Int
        processOneSlope lst shiftRight shiftDown =
            let filtered = allN lst shiftDown in
                processLineByLine filtered shiftRight 0 0

        processLineByLine :: [String] -> Int -> Int -> Int -> Int
        processLineByLine (x:xs) shift depth amount = 
            let position = mod (depth * shift) (length x)
                isTree = (x!!position) == '#'
            in
                processLineByLine xs shift (depth + 1) (if isTree then amount + 1 else amount)
        processLineByLine [] _ _ amount = amount

main :: IO ()
main = getInput >>= \lines -> let res = processLines lines in print res
