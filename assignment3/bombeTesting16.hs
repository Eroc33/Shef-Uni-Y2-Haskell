module Bombe_Testing where

import Bombe
import Data.List (delete)
import Data.Maybe (isJust,fromJust)
 
--  import whatever you need 
 
 {- testing for assignment3-}
 
 {- TEST1: a contrived example for which the solution should be found quickly
    if the enigma is set up as described in the case study and the original message is
 -}
 
p1 = "AIDEGHMC"
 
 -- and the encripted message is
 
x1 = "IDEGHMCL"
 
 -- and the crib is the whole message i.e.
 
c1 = zip p1 x1
 

 {- and the Start position is 0
    then you should find a solution for rotor positions (0,0,0)  
    - i.e. enigmaEncode was called with (0,0,0) so the rotors were at (0,0,1) when
           the first character was encoded
    the stecker discovered will be -}
 
st1 = [('L','P'),('C','Q'),('M','R'),('H','S'),('G','T'),('E','U'),('D','V'),('I','W'),('A','X')]
 
 -- you may also find an erroneous solution with the initial rotor positions (25,25,25) 

 {- to check you can decode x1 to give p1 with 
    enigmaEncodeMessage x1 (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB st1 (0,0,0)
 -}
 
-- is te pair in the stecker, in either direction
inStecker (a,b) stecker = (lookup a stecker == Just b) || (lookup b stecker == Just a)

--remove the pair from the stecker, in both directions
removeSteckerPair (a,b) stecker = delete (b,a) (delete (a,b) stecker)
 
--are the suplied steckerboards equal when treated as a bimap
steckersEqual [] [] = True
steckersEqual [] _ = False
steckersEqual _ [] = False
steckersEqual (pair:a) b = inStecker pair b && steckersEqual a (removeSteckerPair pair b)

--since the expected steckerboard is known we can test their equality, though == will not work
--as we treat the steckerboard as a bimap, rather than a list of tuples
test1 = (show steckerboard)++" == "++(show st1)++"? "++ (show $ isJust found && steckersEqual steckerboard st1)
    where found = breakEnigma (unzip c1) 
          settings = fromJust found
          steckerboard = snd $ settings
 
 
 ------------------------------------------------------------------------------
-- TEST2

 -- the message is known to start with
 
dcs_header = "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP"
 
 -- the encoded message is
 
c2 = "RCQRSVHNYQHLVKLELFYSYCCLMKHUFXMVYVREFLHZOLRCBRHWPQDUONZWOGRTYKAUW"
 
 -- note that you may get a partial decode, as in assignment 1, but you should be able to guess the remaining letters 
 
test2 = fromJust $ decode dcs_header c2
 
 -----------------------------------------------------------------------
 --TEST3

 -- the message starts with 
ht = "TURINGBOMBEHASKELLSIMULATIONSTOP"
 
 -- the encoded message is
 
c3 = "FNWGDVEHEHJXCGOTOHQLELJOAGABOIDLXIGKFISZUZCAQNUWKXUMSWTYMBIDZF"

test3 = fromJust $ decode ht c3

--Test results:

-- *Bombe_Testing> test1
-- "[('P','L'),('Q','C'),('R','M'),('S','H'),('T','G'),('U','E'),('V','D'),('W','I'),('A','X')] == [('L','P'),('C','Q'),('M','R'),('H','S'),('G','T'),('E','U'),('D','V'),('I','W'),('A','X')]? True"

-- The output steckerboard was equivalent to the one that was expected

-- *Bombe_Testing> test2
-- ("COPMUTERSCJENCESHEFFJELDENJVERDJTYSTOMTHJSJSTHETUSTTWOPESSAGESTOM","COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOPTHISISTHETESTTWOMESSAGESTOP")

--  ^Partially decoded message (By simply following menu)               ^Fully decoded message (by using custom stecker improvement function)

-- *Bombe_Testing> test3
-- ("TUZIMGBOMBEHASKELLSIMULATIONSTYPHERESTHEANSDERFXRTESTOHREESZOP","TURINGBOMBEHASKELLSIMULATIONSTOPHERESTHEANSWERFORTESTTHREESTOP")

--  ^Partially decoded message (By simply following menu)            ^Fully decoded message (by using custom stecker improvement function)


-- BONUS: THE SEASONAL(ish) MESSAGE

-- *Bombe> decode "SIXGEESEALAYINGSTOPSEVENSWANSASWIMMING" "HWHYBANIKFUUXIZXMVADOBBEVKNXBGRTCJSBIDURVXSPRCOIPROVEKYVTYDRESBEBOABJXAGGOEHMIHNMTPO"
-- Just ("SICGEESEALAYSNGQTOPSEVENSWAHSASWSMMINGSTOMIFTMEANSWERISFORTYTWOWHATSTJEQUESTIONABOUT","SIXGEESEALAYINGSTOPSEVENSWANSASWIMMINGSTOPIFTHEANSWERISFORTYTWOWHATSTHEQUESTIONABOUT")

--        ^Partially decoded message (By simply following menu)                                  ^Fully decoded message (by using custom stecker improvement function)

-- If the answer is "forty two" the question is about the meaning of life

 
 
                
                
              