module Oblig0 where

import qualified Data.Set as Set
import Data.List (lookup)
import Data.Char
import Data.Char (ord, chr, isLower, isUpper)
import Data.Maybe (fromMaybe)
import Data.Maybe (mapMaybe)
import Data.List (cycle)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import Data.Maybe
import Data.List (nub)
import Data.Char (isAlpha, toLower)
import System.IO
import Data.List (minimumBy)

-- types
type Key = [(Char,Char)]
type FrequencyTable = [(Char,Double)]
type Alphabet = String
type Dictionary = Set.Set String

-- Encoding med map til key
encode :: Key -> String -> String
encode key plaintext = map (maybeEncrypted key) plaintext

-- Sjekker for hvilke element som skal krypteres
maybeEncrypted :: Key -> Char -> Char
maybeEncrypted key c = case lookup c key of
    Just encrypted -> encrypted
    Nothing -> c

-- invertere nøkkelen
invert :: Key -> Key
invert key = [(b, a) | (a, b) <- key]

-- dekrypterer med inversnøkkelen på teksten
decode :: Key -> String -> String
decode key text = encode (invert key) text

-- Flytter verdiene i alfabetet - positive retning, negativ retning og på tall
caesar :: Alphabet -> Integer -> Key
caesar alphabet shift =
    let len = fromIntegral (length alphabet)
        shiftedAlphabet = drop (fromIntegral (shift `mod` len)) (cycle alphabet)
    in zip alphabet shiftedAlphabet

-- teller antall forekomster av hver character i en string
countOccurrence :: String -> Map.Map Char Integer
countOccurrence [] = Map.empty
countOccurrence (c:cs) = Map.insertWith (+) c 1 (countOccurrence cs)

-- gir brøken av forekomster av characters i en string
frequencies :: String -> Map.Map Char Double
frequencies s = fmap (\count -> fromIntegral count / total) (countOccurrence s)
  where
    total = fromIntegral (length s)

-- gir desimaltallet av forekomsten av en character i en string
count :: String -> FrequencyTable
count s = Map.toList $ frequencies s

-- lager en frekvens tabell av en fil som blir lest inn
-- funksjon for å lese filen linje for linje og bygge opp frekvenstabellen
loadFrequencyTable :: FilePath -> IO FrequencyTable
loadFrequencyTable file = do
    contents <- readFile file
    return $ count contents

-- sortere synkende på frekvensverdi
initialGuess :: FrequencyTable -> FrequencyTable -> Key

initialGuess [] _ = []
initialGuess _ [] = []

initialGuess model observed =
    let sortedModel = sortBy (flip (comparing snd)) model
        sortedObserved = sortBy (flip (comparing snd)) observed
        combined = zip sortedModel sortedObserved
    in map (\((m, _), (o, _)) -> (m, o)) combined

-- chiSquared
-- i tilfellet der det ikke er en char i o, skal det være 0
-- i tilfellet der char ikke er i e, skal det brukes smoothing factor, for å sikre at det ikke deles på 0 -> NaN
-- fromMaybe sjekker om det er just eller nothing, fromMaybe (x)

smoothingFactor :: Double -- En smoothingFactor som kan brukes, sikre output
smoothingFactor = 1 / 10000

-- chiSquared funksjonen
chiSquared :: FrequencyTable -> FrequencyTable -> Double
chiSquared observed expected = sum[((obs - exp)^2) / exp | char <- uniqueChars,let obs = fromMaybe 0 (lookup char expected), let exp = fromMaybe smoothingFactor (lookup char observed)]
  where
    uniqueChars = nub (map fst observed ++ map fst expected)
  
-- bruke eq for swap
-- ordner på tupler og generisk - kan man bruke compare uten ord??
-- sortere output par

swapEntries :: Eq a => (a, b) -> (a, b) -> [(a, b)] -> [(a, b)]
swapEntries c1 c2 key = [updateEntry entry | entry <- key]
  where
    updateEntry (currentChar, value)
      | currentChar == fst c1 = (fst c2, value)
      | currentChar == fst c2 = (fst c1, value)
      | otherwise = (currentChar, value)






-- finne beste nabo - se på par og nøkler
-- listekomprhensjon og swapEntries
neighbourKeys :: Key -> [Key]
neighbourKeys key = [swapEntries pair1 pair2 key | (pair1, index1) <- indexedKey, (pair2, index2) <- indexedKey, index1 < index2]
  where
    indexedKey = zip key [0..]

-- grådig funksjon
-- Modify the greedy function to use findBestNeighbourKey
-- Greedy algorithm to minimize chi-squared with given model and ciphertext
greedy :: FrequencyTable -> String -> Key -> Key
greedy model ciphertext currentKey =
  let chiSquaredCurrent = chiSquared model (count (decode currentKey ciphertext))
      bestKey = findBestNeighbourKey model ciphertext currentKey
  in
    if chiSquared model (count (decode bestKey ciphertext)) < chiSquaredCurrent
    then
      greedy model ciphertext bestKey  -- If the new key improves chi-squared, continue with it
    else
      currentKey  -- Otherwise, keep the current key

findBestNeighbourKey :: FrequencyTable -> String -> Key -> Key
findBestNeighbourKey model ciphertext currentKey =
  minimumBy (comparing (\key -> chiSquared model (count (decode key ciphertext)))) (neighbourKeys currentKey)

-- Lese inn fra fil, lignende loadFrequencyTable
-- bruke words og Set.fromList
loadDictionary :: FilePath -> IO Dictionary
loadDictionary file = do
    contents <- readFile file
    let wordList = words contents
    return $ Set.fromList wordList

-- filter (Set.member)
-- husk fromIntegral på int length
countValidWords :: Dictionary -> String -> Integer
countValidWords dictionary text =
    let vocabulary = words text
        isValid = filter (\word -> Set.member word dictionary) vocabulary in fromIntegral (length isValid)

-- samme oppsett som greedy med rekursjon og funksjonene neighbourKeys
-- men countValidator og maximum istedenfor chiSquared og minimum
greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dict ciphertext key =
  let currentCount = countValidWords dict (decode key ciphertext)
      neighbors = neighbourKeys key
      (bestKey, bestCount) = findBestKey dict ciphertext neighbors key currentCount
  in if bestCount > currentCount
       then greedyDict dict ciphertext bestKey
       else key

-- funksjon for å finne den beste nøkkelen blant naboene
-- rekursjon
findBestKey :: Dictionary -> String -> [Key] -> Key -> Integer -> (Key, Integer)
findBestKey _ _ [] bestKey bestCount = (bestKey, bestCount)
findBestKey dict ciphertext (k:ks) bestKey bestCount =
  let currentCount = countValidWords dict (decode k ciphertext)
  in if currentCount > bestCount
       then findBestKey dict ciphertext ks k currentCount
       else findBestKey dict ciphertext ks bestKey bestCount