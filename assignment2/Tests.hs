{-# LANGUAGE TemplateHaskell #-}
module Tests where

import AssignmentHelp
import Enigma
import Cipher
import Cribs
import Test.QuickCheck
import Data.List
import Data.Maybe
import Data.Monoid

--this is just apparatus to generate test inputs, scroll down to line 90

genPermutation :: Eq a => [a] -> Gen [a]
genPermutation [] = return []
genPermutation series = do
    element <- elements series
    tail <- genPermutation (delete element series)
    return (element:tail)
    
genAllPermutationPairs :: Eq a => [a] -> Gen [(a,a)]
genAllPermutationPairs [] = return []
genAllPermutationPairs (single:[]) = error "Tried to generate permuted pairs from odd length list"
genAllPermutationPairs series = do
    first <- elements series
    second <- elements (delete first series)
    tail <- genAllPermutationPairs (delete second (delete first series))
    return ((first,second):tail)
    
genSomePermutationPairs :: Eq a => [a] -> Gen [(a,a)]
genSomePermutationPairs [] = return []
genSomePermutationPairs (single:[]) = error "Tried to generate permuted pairs from odd length list"
genSomePermutationPairs series = do
    first <- elements series
    second <- elements (delete first series)
    let tail = genSomePermutationPairs (delete second (delete first series))
    maybeTail <- oneof [tail,return []]
    return ((first,second):maybeTail)

genCipher :: Gen Cipher
genCipher = do
    text <- genPermutation alphabet
    return $ Cipher text


genRotor = do
    offset <- choose (0,25)
    cipher <- genCipher
    return (cipher,offset)
    
genRotors :: Gen Rotors
genRotors = vectorOf 3 genRotor

--reflector must be a full mapping
genReflector :: Gen Reflector
genReflector = genAllPermutationPairs alphabet

--steckerBoard can be a partial mapping
genSteckerBoard :: Gen SteckerBoard
genSteckerBoard = genSomePermutationPairs alphabet
    
genSimpleEnigma :: Gen Enigma
genSimpleEnigma = do
    rotors <- genRotors
    reflector <- genReflector
    return $ SimpleEnigma (rotors,reflector)

genSteckeredEnigma :: Gen Enigma    
genSteckeredEnigma = do
    rotors <- vectorOf 3 genRotor
    reflector <- genReflector
    steckerBoard <- genSteckerBoard
    return $ SteckeredEnigma (rotors,reflector,steckerBoard)

genEnigma :: Gen Enigma 
genEnigma = do
    i<-elements [True,False]
    if i then
        genSteckeredEnigma
    else
        genSimpleEnigma
        
genAlphabetic :: Gen Char         
genAlphabetic = do
    elements alphabet
    
genMessage :: Gen String
genMessage = listOf genAlphabetic

---------------------------
--actual tests begin here--
---------------------------

--Tests are run via quickcheck which generates 100 random input sets per property test on each run 

--check single character reflecting is symmetric
prop_SteckerReverseable = forAll genAlphabetic $ \c -> forAll genReflector $ \r -> reflectorEncode r (fromJust $ reflectorEncode r c) == Just c

--check single character steckering is symmetric
prop_ReflectReverseable = forAll genAlphabetic $ \c -> forAll genSteckerBoard $ \s -> steckerEncode s (steckerEncode s c) == c

--check single character encoding is symmetric
prop_EnigmaEncodeReverseable = forAll genAlphabetic $ \c -> forAll genEnigma $ \e -> (fromJust $ encodeFunc e (fromJust $ encodeFunc e c)) == c

--check string encoding is symmetric
prop_EnigmaEncodeStringReverseable = forAll genMessage $ \m -> forAll genEnigma $ \e -> (enigmaEncodeString e (enigmaEncodeString e m)) == m

prop_RotorAdvanceIsModulo = forAll genRotor $ \r -> (snd $ maybeRotateRotor r True) == (if snd r == 25 then 0 else snd r + 1)

--This test is comparitively slow (due to calling 26^3 functions presumably),
--but this is the best way to test this that I can think of which doesn't
--simply duplicate the original rotor rotation logic
prop_AdvanceRotorsWraps = forAll genRotors $ \r -> (wrapAdvanceRotors r) == r
    where composeN n = appEndo . foldMap Endo . replicate n
          --this fully rotates all the rotors, which should be the same as not rotating
          wrapAdvanceRotors = composeN (26^3) advanceRotors

--runTests will run all quickcheckTests
return []
runTests = $quickCheckAll

-------------------------
--non-quickcheck tests---
-------------------------

--whether we get a longests menu length of 17 for the example crib
exampleLongestMenuLengthMatches = length (longestMenu exampleCrib) == 17

--whether we get 'A' by encoding 'N' with the example enigma
exampleEnigmaSingleCharEncodingMatches = (fst $ enigmaEncode 'N' exampleEnigma) == Just 'A'
