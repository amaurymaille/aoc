module Main where 

import Data.List.Split

data Element = Element Int Int Char [Char]

readInput :: IO [String]
readInput = readFile "2.txt" >>= \content -> return $ lines content

linesToElements :: [String] -> IO [Element]
linesToElements lst = return $ map lineToElement lst
    where
        lineToElement :: String -> Element
        lineToElement str =
            let tokens = words str -- [int-int, char:, [char]]
                delimTokens = map read $ splitOn "-" (tokens!!0)
                char = head $ splitOn ":" (tokens!!1)
            in
                Element (delimTokens!!0) (delimTokens!!1) (char!!0) (head $ reverse tokens)


processElements :: [Element] -> Int
processElements lst = length $ filter isValidElement lst
    where
        isValidElement :: Element -> Bool
        isValidElement (Element low high char content) = 
            let n = length $ filter (==char) content in
            n >= low && n <= high

processElements2 :: [Element] -> Int
processElements2 lst = length $ filter isValidElement lst
    where
        isValidElement :: Element -> Bool
        isValidElement (Element low high char content) = 
            let n = length content
                lowChar = content!!(low - 1)
                highChar = content!!(high - 1) 
            in
                if ((low - 1) >= n) || ((high - 1) >= n) then
                    False
                else
                    ((lowChar == char) && (highChar /= char)) ||
                    ((lowChar /= char) && (highChar == char))

main :: IO ()
main = readInput >>= \lines -> linesToElements lines >>=
                     \elements -> print (processElements elements,
                                         processElements2 elements) 
