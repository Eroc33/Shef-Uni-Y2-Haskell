module Cipher where

import Data.List
import Data.Maybe

data Cipher = Cipher String deriving (Show, Eq)

-- the set of letter's the cipher can/must include
alphabet = ['A'..'Z']

validateCipher :: Cipher -> Bool
validateCipher (Cipher cipher) = sort cipher == alphabet

-- shift list left by n elements
rotate :: [a] -> Int -> [a] 
rotate list n = rotate' list (-n)
rotate' :: [a] -> Int -> [a] 
rotate' list n | (length list) < n = rotate' list (mod n (length list))
              | n < 0 = rotate' list ((length list)-n)
              | otherwise = (drop n list)++(take n list)

encode :: Char -> Cipher -> Int -> Maybe Char
encode plain (Cipher cipher) offset = fmap (rotate cipher offset !!) (elemIndex plain alphabet)

encodeMessage :: String -> Cipher -> Int -> [Char]
encodeMessage plainText cipher offset = catMaybes (fmap (\x -> encode x cipher offset) plainText)

reverseEncode :: Char -> Cipher -> Int -> Maybe Char
reverseEncode ciphText (Cipher cipher) offset = fmap (alphabet !!) (elemIndex ciphText (rotate cipher offset)) 

reverseEncodeMessage :: String -> Cipher -> Int -> [Char]
reverseEncodeMessage ciphText cipher offset = catMaybes (fmap (\x -> reverseEncode x cipher offset) ciphText)

sortBySecond :: Ord a => [(b,a)] -> [(b,a)]
sortBySecond = sortBy (\(_,x) -> \(_,y) -> compare y x)

textPct :: Char -> [Char] -> Float
textPct char text = 
        let count = (length (elemIndices char text))
        in (fromIntegral count)*100.0/(fromIntegral(length text))

letterStats ciphText = 
        sortBySecond (filter ((>0) . snd) (fmap (\x -> (x,textPct x ciphText))  alphabet))

partialDecode guesses ciphText = fmap (\x -> fromMaybe x (lookup x guesses) ) ciphText
