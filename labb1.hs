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


-- Uppgift 3

-- Function that counts the number of words in a String by checking for character 
-- Followed by a space.

countWords :: String -> Double
countWords [] = 0
countWords (x:xs)
        | (isAlpha x == True && xs == []) = 1 + countWords xs
        | (isAlpha x == True) && ( isAlpha (head(xs)) == False) = 1 + countWords xs
        | otherwise = countWords xs

-- Function that counts the number of characters in a String by checking if 
-- every value is True or False when passed to the isAlpha function. 


countCharacters :: String -> Double
countCharacters [] = 0
countCharacters (x:xs)
        | (isAlpha x == True) = 1 + countCharacters xs
        | otherwise = countCharacters xs


-- Function that checks the average length of a word in a sentence by dividing
-- number of characters by the number of words.
medellangd :: String -> Double
medellangd [] = 0
medellangd x = countCharacters x/countWords x