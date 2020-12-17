module Main where

getInput :: IO [String]
getInput = readFile "3.txt" >>= \content -> return $ lines content

printInput :: [String] -> IO ()
printInput (x:xs) = print x >> 
                    printInput xs
printInput [] = return ()

processLines :: [String] -> Int
processLines lines = processLineByLine lines 0 0
    where 
        processLineByLine :: [String] -> Int -> Int -> Int
        processLineByLine (x:xs) depth amount = 
            let position = mod (depth * 3) (length x)
                isTree = (x!!position) == '#'
            in
                processLineByLine xs (depth + 1) (if isTree then amount + 1 else amount)
        processLineByLine [] _ amount = amount

main :: IO ()
main = getInput >>= \lines -> let res = processLines lines in print res
