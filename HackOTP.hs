import Numeric (readHex)
import Data.Char (ord, chr)
import Data.List (find)
import Data.Bits (xor)
import Control.Monad (liftM)

printUsage = do
  putStrLn "Usage: First off you will input the number of ciphers that has been encrypted with the same key. These must be the same length."
  putStrLn "This will be followed by the input of the ciphers, hex encoded."
  putStrLn "The ciphers will be printed with the first found key where all the inputs follows the rules."
  putStrLn "The next step is manual. You enter the 1-indexed index of a character that seems out of place. If there are alternatives it will be replaced. If you change one by accident you can prefix the number with a minus sign (-). Repeat until all the decrypted texts seems right."


hexToBin [] = []
hexToBin (x:x2:xs) = (chr $ fst $ (readHex $ x:x2:[]) !! 0) : hexToBin xs

validCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ " .!?"

isValid x = find (\y -> y == x) validCharacters /= Nothing

combinations [] = []
combinations (_:[]) = []
combinations (x:xs) = (map (\y -> (x, y)) xs) ++ combinations xs

isValidKey list key = find (\x -> not $ isValid $ chr $ xor (ord $ x !! 0) $ ord key) list == Nothing

findAllKeys [] = []
findAllKeys (_:[]) = []
findAllKeys ("":_) = []
findAllKeys strings@(first:rest) = findAllKeys_:findAllKeys (map tail strings) 
  where
  findAllKeys_ = do
    actuallyValid <- filter (isValidKey rest) $ map (\x -> chr $ xor (ord x) (ord $ first !! 0)) validCharacters
    return actuallyValid

decrypt key strings = map (\string -> zipWith (\x y -> chr $ xor (ord x) (ord y)) key string) strings

replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

printStrings [] = return ()
printStrings (x:xs) = do
  putStrLn x
  printStrings xs

findCorrectKey keys strings corrections lastModification = do
  putStrLn $ (replicate ((abs lastModification) - 1) ' ') ++ "| " ++ (show lastModification)
  let key = (zipWith (\keys index -> keys !! index) keys corrections)
  printStrings $ decrypt key strings
  putStrLn $ (replicate ((abs lastModification) - 1) ' ') ++ "| " ++ (show lastModification)
  --putStrLn $ show (zipWith (\x y -> (y, x)) corrections [1..(length corrections)])
  putStrLn $ "Key: " ++ (show $ map ord key)
  input <- getLine
  let modification_ = if input == "" then lastModification
                                     else read input :: Int
  let modification = (abs modification_) - 1
  let modifyBy = if modification_ > 0 then 1
                                      else -1
  if ((length (keys !! modification)) > (corrections !! modification) + modifyBy) &&
    corrections !! modification + modifyBy >= 0
    then findCorrectKey keys strings (replaceNth modification (modifyBy + (corrections !! modification)) corrections) modification_
    else findCorrectKey keys strings corrections lastModification

main = do
  printUsage
  putStrLn "How many ciphers?"
  count_ <- getLine
  let count = read count_ :: Int
  putStrLn "Input the ciphers in hex encoding"
  lines <- mapM (\_ -> getLine) [1..count]
  let decodedLines = map hexToBin lines
  let allKeys = findAllKeys decodedLines
  findCorrectKey allKeys decodedLines (replicate (length $ decodedLines !! 0) 0) 1

