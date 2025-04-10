This Haskell module implements a variety of cryptographic algorithms and tools for frequency analysis, focusing on classical ciphers like substitution and Caesar ciphers.
It includes functionality for encoding, decoding, and performing cryptanalysis using methods like Chi-squared analysis and greedy algorithms.
It support frequency analysis and dictionary-based techniques for decryption.

____________________________________________________________________________________________________

Features
Substitution Cipher: Encode and decode messages using substitution ciphers.

Caesar Cipher: Generate and use Caesar cipher keys to shift characters in an alphabet.

Frequency Analysis: Analyze and generate frequency tables for characters in a string or file.

Cryptanalysis: Use Chi-squared minimization and greedy algorithms to break ciphers.

Dictionary-based Cryptanalysis: Decode messages by matching them with valid words from a dictionary.

____________________________________________________________________________________________________

Installation
To use this module, you need to have GHC (Glasgow Haskell Compiler) installed.
If you don't have GHC installed, you can follow the instructions on the official GHC website.

1. Clone this repository
   git clone <repository-url>
   
2. Navigate to the project directory:
   cd <project-directory>
   
3. Compile the project:
   ghc --make Oblig0.hs

4. Run the compiled executable (replace Oblig0 with your desired filename):
   ./Oblig0

____________________________________________________________________________________________________

USAGE

Encode a Message
Use the encode function to encode a message with a substitution cipher.

```
Haskell: 
let key = caesar "abcdefghijklmnopqrstuvwxyz" 3
let encoded = encode key "hello"
-- encoded will be "khoor"

Decode a Message
Use the decode function to decode a message with a given key.

Haskell:
let decoded = decode key "khoor"
-- decoded will be "hello"

Frequency Analysis
Analyze the frequency of characters in a string or file:

let text = "some example text"
let frequencyTable = count text
-- frequencyTable will be a list of characters and their frequencies

```
Cryptanalysis (Greedy Algorithm)
Use the greedy function to perform cryptanalysis on a ciphertext based on a frequency model (English letter frequencies):

let model = loadFrequencyTable "english_freq.txt"
let ciphertext = "gsv jfrxp yildm ulc"
let bestKey = greedy model ciphertext initialKey
-- bestKey will be the most likely decryption key

Dictionary-based Cryptanalysis
Use a dictionary of valid words to improve decryption:

let dictionary = loadDictionary "dictionary.txt"
let bestKey = greedyDict dictionary ciphertext initialKey
-- bestKey will be the decryption key that maximizes the number of valid words



