{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (w:ws) hnd
	| w `notElem` hnd = False
	| w `elem` hnd = formableBy ws (delete w hnd)


wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate [] _ _ = False
wordFitsTemplate _ _ [] = False
wordFitsTemplate (t:ts) hn (wr:wrs)
	| wr == t 						= wordFitsTemplate ts hn wrs
	| wr `elem` hn && t == '?'		= wordFitsTemplate ts (delete wr hn) wrs
	| otherwise = False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate tmp hnd = filter (wordFitsTemplate tmp hnd) allWords
--wordsFittingTemplate tmp hnd = filter (`wordFitsTemplate` tmp hnd) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord [] = 0
scrabbleValueWord (s:ss) = scrabbleValue s + scrabbleValueWord ss

bestWords :: [String] -> [String]
bestWords pos = bestWords' (maximum(bestWords'' pos)) (bestWords'' pos) pos

-- helper which does scrabbleValueWord onto words and puts them in a list
bestWords'' :: [String] -> [Int]
bestWords'' [] = []
bestWords'' (wrd:wrds) = scrabbleValueWord wrd : [] ++ bestWords'' wrds 

-- accumulator
bestWords' :: Int -> [Int] -> [String] -> [String]
bestWords' maxi [] [] = []
bestWords' maxi (x:xs) (y:ys)
	| x == maxi = bestWords' maxi xs ys ++ y : []
	| otherwise = bestWords' maxi xs ys

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate sTemp word = (scrabbleValueTemplate'' sTemp 1) * (scrabbleValueTemplate' sTemp word)

scrabbleValueTemplate' :: STemplate -> String -> Int
scrabbleValueTemplate' [] [] = 0
scrabbleValueTemplate' (t:ts) (s:ss)
	| t == 'D' = (scrabbleValue s * 2) + scrabbleValueTemplate' ts ss
	| t == 'T' = (scrabbleValue s * 3) + scrabbleValueTemplate' ts ss
	| otherwise = scrabbleValue s + scrabbleValueTemplate' ts ss

scrabbleValueTemplate'' :: STemplate -> Int -> Int
scrabbleValueTemplate'' [] multi = multi
scrabbleValueTemplate'' (tp:tps) multi
	| tp == '2' = scrabbleValueTemplate'' tps (multi * 2)
	| tp == '3' = scrabbleValueTemplate'' tps (multi * 3)
	| otherwise = scrabbleValueTemplate'' tps multi

