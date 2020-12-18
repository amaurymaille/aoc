module Main where

import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Read

processInput :: [String] -> (Int, Int)
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

        processGroups :: [[String]] -> (Int, Int)
        processGroups groups = let res = map processGroup groups in (sum $ map fst res, sum $ map snd res)

        processGroup :: [String] -> (Int, Int)
        processGroup group =
            let n = length group in
                if n == 8 then 
                    (1, validateGroup group)
                else if n == 7 then
                    let cidMissing = isCidMissing group in (cidMissing, if cidMissing == 1 then validateGroup group else 0)
                else
                    (0, 0)
            where 
                isCidMissing :: [String] -> Int
                isCidMissing group = if any isCid group then 0 else 1

                isCid :: String -> Bool
                isCid str = head (splitOn ":" str) == "cid"

                validateGroup :: [String] -> Int
                validateGroup group = if all validateElement group then 1 else 0
                    where
                        validateElement :: String -> Bool
                        validateElement elem = 
                            let component = splitOn ":" elem 
                                name = head component
                                value = head $ tail component
                            in
                                validate name value
                            where
                                -- Should probably use parsed / megaparsec
                                validate :: String -> String -> Bool

                                validate "byr" value = validateYear value 1920 2002
                                validate "iyr" value = validateYear value 2010 2020
                                validate "eyr" value = validateYear value 2020 2030
                                validate "hgt" value =
                                    let parseHeight = many digit >>= \digits -> (string "in" <|> string "cm") >>= \num -> eof >> return (digits, num)
                                        result = parse parseHeight "" value
                                    in
                                        case result of 
                                            Left _ -> False
                                            Right (digits, num) -> let asInt = read digits :: Int in 
                                                                    if num == "cm" then
                                                                        asInt >= 150 && asInt <= 193
                                                                    else
                                                                        asInt >= 59 && asInt <= 76
                                validate "hcl" value = 
                                    length value == 7 && head value == '#' && all validateEcl (tail value)
                                    where
                                        validateEcl :: Char -> Bool
                                        validateEcl ecl = (ecl >= 'a' && ecl <= 'f') || (ecl >= '0' && ecl <= '9')
                                validate "ecl" value = 
                                    isJust $ find (== value) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                                validate "pid" value =
                                    length value == 9 && isJust (readMaybe value :: Maybe Int)
                                validate "cid" value = True
                                validate _ value = False

                                validateYear :: String -> Int -> Int -> Bool
                                validateYear year low up = let parseYear = many digit 
                                                               asInt = parse parseYear "" year 
                                                           in 
                                                            case asInt of 
                                                                Left _ -> False
                                                                Right yearAsInt -> let y = read yearAsInt :: Int in y >= low && y <= up

main :: IO ()
main = readFile "4.txt" >>= \content -> print (processInput $ lines content)
