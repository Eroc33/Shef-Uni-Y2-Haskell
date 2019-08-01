module Enigma where

import Cipher
import Control.Monad
import Data.Maybe
import Data.Tuple
import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Functor.Identity
import AssignmentHelp hiding (fromMaybe)


type Reflector = [(Char,Char)]

type SteckerBoard = [(Char,Char)]

type Rotor = (Cipher,Int)

--[lr,mr,rr] using a list reduces boilerplate over using a tuple as we can map it
--(it also allows more than 3 rotors if desired)
type Rotors = [Rotor]

data Enigma = SimpleEnigma(Rotors,Reflector)
            | SteckeredEnigma(Rotors,Reflector,SteckerBoard) deriving Show

standardReflector :: Reflector
standardReflector = [('A', 'Y'),('B', 'R'),('C', 'U'),('D', 'H'),('E', 'Q'),('F', 'S'),('G','L'),('I', 'P'),('J', 'X'),('K', 'N'),('M', 'O'),('T', 'Z'),('V','W')]

exampleEnigma = SimpleEnigma([(Cipher rotor1,25),(Cipher rotor2,25),(Cipher rotor3,25)],standardReflector)

rotorEncode (cipher,offset) letter = encode letter cipher offset
rotorReverseEncode (cipher,offset) letter = reverseEncode letter cipher offset

reflect :: [(Char,Char)] -> Char -> Maybe Char
reflect subst letter = (lookup letter subst) <|> (lookup letter (fmap swap subst))

steckerEncode :: SteckerBoard -> Char -> Char
steckerEncode stecker letter = fromMaybe letter (reflect stecker letter)

reflectorEncode = reflect

incomingRotors rotors = foldl (>=>) (return) (fmap rotorEncode rotors)
outgoingRotors rotors = foldr (<=<) (return) (fmap rotorReverseEncode rotors)

--return rotated rotor if condition is true else return the original rotor
maybeRotateRotor :: Rotor -> Bool -> Rotor
maybeRotateRotor (ciph,offset) rotate = (ciph,newOffset)
    where newOffset = (offset + (if rotate then 1 else 0)) `mod` 26

--advance a list of rotors, depending on whether the previous rotor was advanced
advanceRotors :: Rotors -> Rotors
advanceRotors rotors = fmap (uncurry maybeRotateRotor) $ zip rotors $ rotorShouldRotate
    where rotorShouldRotate = drop 1 $ fmap ((==25) . snd) rotors ++ [True]

--encode a single character with the given enigma
encodeFunc :: Enigma -> Char -> Maybe Char
encodeFunc (SimpleEnigma(rotors, reflector)) = (outgoingRotors rotors) <=< (reflectorEncode reflector) <=< (incomingRotors rotors)
-- encoding for a steckered enigma is as simple, but passed through the steckerboard before and after
encodeFunc (SteckeredEnigma(rotors, reflector, steckerboard)) = (return . steckerEncode steckerboard) <=< (encodeFunc (SimpleEnigma(rotors,reflector))) <=< (return . steckerEncode steckerboard)

--return the next enigma state
advanceEnigma (SimpleEnigma(rotors,reflector)) = SimpleEnigma(advanceRotors rotors,reflector)
advanceEnigma (SteckeredEnigma(rotors,reflector,steckerboard)) = SteckeredEnigma(advanceRotors rotors,reflector,steckerboard)

--encode a single character with the given enigma, and return the next enigma state
enigmaEncode :: Char -> Enigma -> (Maybe Char, Enigma)
enigmaEncode letter enigma = (encode letter,advanced)
    where encode  = encodeFunc advanced
          advanced = advanceEnigma enigma
                
enigmaEncodeStringM :: String -> State Enigma String
enigmaEncodeStringM string = do
        encoded <- enigmaEncodeString' string
        return $ catMaybes $  encoded
    where enigmaEncodeString' :: String -> State Enigma [Maybe Char]
          enigmaEncodeString' [] = return []
          enigmaEncodeString' string = do
            let(head:tail) = string
            letter <- state $ enigmaEncode head
            do
                    rest <- enigmaEncodeString' tail
                    return (letter:rest)
    
enigmaEncodeString :: Enigma -> String -> String
enigmaEncodeString enigma string = fst $ runIdentity $ runStateT (enigmaEncodeStringM string) enigma

