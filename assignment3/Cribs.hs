module Cribs where

import Data.List
import Data.Maybe
import Data.Function (on)

type Crib =  (String, String)
type Menu = [Int]

defaultCrib = ("WETTERVORHERSAGEBISKAYA","RWIVTYRESXBFOGKUHQBAISE")

links :: Crib -> [(Int,Int)]
links (plain, cipher) = concat [ zip (repeat i) (elemIndices c plain) | (c,i) <- zip cipher [0..] ]

follows a b = fst a == snd b

followers chain@(current:_) rest unvisitedIndices link | link `follows` current && (snd link) `elem` unvisitedIndices = findChains' (link:chain) (delete link rest) (delete (snd link) unvisitedIndices)
followers chain@(current:_) rest unvisitedIndices link | otherwise = [chain]
          
-- returned chains are actually reversed for easier extending
findChains' :: [(Int,Int)] -> [(Int,Int)] -> [Int] -> [[(Int,Int)]]
findChains' chain [] unvisitedIndices = [chain]
findChains' chain rest unvisitedIndices = nub $ concat $ fmap mapping rest
    where mapping = followers chain rest unvisitedIndices

findChains :: [(Int,Int)] -> Int -> [[(Int,Int)]]
findChains links len = concat $ fmap (\link -> findChains' [link] (delete link links) (delete (fst link) (delete (snd link) [0..len]))) links

linksChainsToMenus :: [[(Int,Int)]] -> [[Int]]
linksChainsToMenus chains = fmap (reverse . linkChainToMenu) chains
    where linkChainToMenu :: [(Int,Int)] -> [Int]
          linkChainToMenu (last:[]) = [snd last, fst last]
          linkChainToMenu (link:rest) = (snd link):(linkChainToMenu rest)
       
longest :: (Foldable t0, Foldable t1, Ord a) => t0 (t1 a) -> t1 a
longest foldable = maximumBy (compare `on` length) foldable
          
longestMenu :: Crib -> [Int]
longestMenu crib@(plain,_) = longest $ linksChainsToMenus $ findChains (links crib) (length plain)
