import Data.Char
import Data.List

-- Uppgift 2

rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (x:xs)
    | elem x "aeiouy" = x : (rovarsprak xs)
    | otherwise = x : 'o' : x : (rovarsprak xs)


karpsravor :: String -> String
karpsravor [] = []
karpsravor (x:xs)
    | elem x ("aeiouy") = x : karpsravor xs
    | otherwise = x : karpsravor (tail (tail xs))


