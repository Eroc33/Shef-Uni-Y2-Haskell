module Bombe where

import AssignmentHelp hiding (fromMaybe)
import Enigma
import Cribs
import Cipher
import Data.Tuple
import Data.Maybe
import Data.Char
import Data.List
import Control.Applicative
import Control.Monad

import Debug.Trace

type Offsets = (Int,Int,Int)
type SteckerPair = (Char,Char)

breakEnigma :: Crib -> Maybe (Offsets,SteckerBoard)
breakEnigma = breakEnigmaEx standardRotors standardReflector
          
--break enigma without limiting to standard rotors and reflector
breakEnigmaEx :: (Cipher,Cipher,Cipher) -> Reflector -> Crib -> Maybe (Offsets,SteckerBoard)
breakEnigmaEx rotors reflector crib = result
    where menu@(i:_) = longestMenu crib
          rotors' = offsetRotors rotors (0, 0, 0)
          initialStecker = [((fst crib) !! i,(fst crib) !! i)]
          result = breakEA reflector crib menu initialStecker rotors'

standardRotors = ((Cipher rotor1),(Cipher rotor2),(Cipher rotor3))

offsetRotors (rl,rm,rr) (ol,om,or) = [(rl,ol),(rm,om),(rr,or)]

getOffsets rotors = (snd $ rotors !! 0, snd $ rotors !! 1, snd $ rotors !! 2)
          
advanceRotorsN :: Int -> Rotors -> Rotors
advanceRotorsN 0 rotors = rotors
advanceRotorsN n rotors = advanceRotorsN (n-1) (advanceRotors rotors)

steckerAdd :: SteckerPair -> SteckerBoard -> Maybe SteckerBoard
steckerAdd (r,c) stecker | valid = Just ((r,c):stecker)
                         | alreadyIn = Just stecker
                         | otherwise = Nothing
        where valid = ((notInStecker stecker r) && (notInStecker stecker c))
              alreadyIn = steckerEncode stecker r == c
              notInStecker stecker letter = (lookup letter stecker == Nothing) && (lookup letter (fmap swap stecker) == Nothing)
          
followMenu :: Reflector->Crib->Menu->SteckerBoard->Rotors->Maybe SteckerBoard
followMenu reflector crib [] stecker rotors = Just stecker
followMenu reflector crib (i:menu) stecker rotors | isJust next = followMenu reflector crib menu (fromJust next) rotors
                                        | otherwise = Nothing
    where next | nextValid = steckerAdd (fromJust r,c) stecker
               | otherwise = Nothing
          nextValid = isJust r
          p = (fst crib) !! i
          q = steckerEncode stecker p
          r = fst $ enigmaEncode q $ SimpleEnigma (advanceRotorsN i rotors,reflector)
          c = (snd crib) !! i
          
--adds a (wrapping) offset to uppercase character
offsetChar c i = chr $ wrap alphaMin alphaMax (ord c + i)
    where alphaMin = ord 'A'
          alphaMax = ord 'Z'
          wrap min max n = (n - min) `mod` (max-min+1) + min
          
findStecker :: Reflector -> Crib -> Menu -> SteckerBoard -> Rotors -> Maybe SteckerBoard
findStecker reflector crib menu [(x,y)] rotors = listToMaybe $ catMaybes [followMenu reflector crib menu [(x,offsetChar y charOffset)] rotors | charOffset <- [0..25]]

breakEA ::  Reflector ->  Crib -> Menu -> SteckerBoard -> Rotors-> Maybe (Offsets, SteckerBoard)
breakEA reflector crib menu initialStecker rotors = listToMaybe $ catMaybes $ breakEA' rotors 0
          --utility function to hide rotor offset state from the caller during recursion
    where breakEA' :: Rotors -> Int -> [Maybe (Offsets,SteckerBoard)]
          -- stop at 26^3 = 17576, as at that point rotors combinations are exhausted
          breakEA' rotors 17576 = []
          breakEA' rotors offset = (fmap ((,) (getOffsets rotors)) $ findStecker reflector crib menu initialStecker rotors):(breakEA' (advanceRotors rotors) (offset+1))



--below this point are extra functions to try and guess additional (non menu) steckering

zipWithIndex f a b = zipWith3 f [0..] a b

indexedCrib (plain,cipher) = zipWithIndex (,,) plain cipher

--try adding both forward and backwards deduced stecker pairs for given rotors, reflector, index, and plain and cipher characters
scanSteckerOpts rotors reflector stecker (i,p,c) = fromMaybe stecker ((r >>= (\r -> steckerAdd (r,c) stecker)) <|> (r' >>= (\r' -> steckerAdd (r',p) stecker)))
        -- q & r deduced as in follow menu, and q' & r' deduced by reversing the same reasoning used for q & r
    where q = steckerEncode stecker p
          r = fst $ enigmaEncode q $ SimpleEnigma (advanceRotorsN i rotors,reflector)
          q' = steckerEncode stecker c
          r' = fst $ enigmaEncode q' $ SimpleEnigma(advanceRotorsN i rotors,reflector)

--scan through the crib to try and deduce stecker entries for unsolved pairs
improveStecker stecker rotors reflector crib = foldl (scanSteckerOpts rotors reflector) stecker (indexedCrib crib)

--break the enigma, then also attempt to improve the resulting steckerboard suing the rest of the crib
decode plain cipher = decodeEx plain cipher standardReflector standardRotors

--decode without limiting to standard rotors and reflector
decodeEx plain cipher reflector rotors  = do
    let crib = unzip $ zip plain cipher
    settings <- breakEnigma crib
    let rotors' = offsetRotors rotors $ fst settings
    let stecker = snd settings
    let enigma = SteckeredEnigma(rotors',reflector,stecker)
    let decoded = enigmaEncodeString enigma cipher
    let improvedStecker = improveStecker stecker rotors' reflector crib
    let improvedEnigma = SteckeredEnigma(rotors',reflector,improvedStecker)
    let improvedDecoded = enigmaEncodeString improvedEnigma cipher
    traceShow (stecker,improvedStecker) (Just 1)
    return (decoded,improvedDecoded)