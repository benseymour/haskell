{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW01 where         -- We'll learn more about this later

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.


lastDigit	:: Integer -> Integer
lastDigit lst = lst `mod` 10

dropLastDigit  :: Integer -> Integer
dropLastDigit frst = frst `div` 10

-- Turn int into list of seperate ints
-- Why does this return reversed list? lastDigit int : [] ++ toDigits (dropLastDigit int)
toDigits :: Integer -> [Integer]
toDigits int
		| int < 1 	= []
		| int >= 1 	= toDigits (dropLastDigit int) ++ lastDigit int : []

-- Doubles every other number from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther ltd
		| length ltd `mod` 2 == 1 		= head ltd : doubleEveryOther (tail ltd)
		| length ltd `mod` 2 == 0		= (head ltd) * 2 : doubleEveryOther (tail ltd)

-- Adds all digits in array together
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits x = sumDigitsInInt (head x) + sumDigits (tail x)


sumDigitsInInt :: Integer -> Integer
sumDigitsInInt n
		| n < 1 = 0
		| otherwise = lastDigit n + sumDigitsInInt(dropLastDigit n)