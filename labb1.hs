module F1 where
import Data.Char
import Data.List

-- Uppgift 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib 3 = 2
fib 4 = 3
fib 5 = 5
fib x = 5 * fib(x-6) + 8 * fib(x-5)

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


-- Uppgift 4

skyffla :: [a] -> [a] 
skyffla [] = []
skyffla x = sortOddIndex x ++ skyffla(sortEvenIndex x) 

sortOddIndex :: [a] -> [a]
sortOddIndex [] = []
sortOddIndex xs = [(xs !! i) | i <- [0..length(xs)-1], i `mod` 2 == 0] 

sortEvenIndex :: [a] -> [a]
sortEvenIndex [] = []
sortEvenIndex xs = [(xs !! i) | i <- [0..length(xs)-1], (i+1) `mod` 2 == 0]  