module ListManipulation where

myCurry = "Curry is awesome!"

dropLast :: [a] -> [a]
dropLast x = take (length x - 1) x

lastOfFirstWord :: String -> Char
lastOfFirstWord x = (\x -> x !! (length x - 1)) $ takeWhile (/= ' ') x

lastWord :: String -> String
lastWord x = reverse $ takeWhile (/= ' ') $ reverse x

takeThird :: [a] -> a
takeThird x = x !! 2

letterIndex :: Int -> Char
letterIndex x = myCurry !! x
