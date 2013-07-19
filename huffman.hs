import Data.Word
import qualified Data.ByteString as BS

data BTree a = Leaf a | Node (BTree a) (BTree a) deriving (Eq, Show)
type Tup a = (BTree a, Int)
type PQueue a = [Tup a]

add :: Tup a -> PQueue a -> PQueue a
add a b = add_w a [] b
   where
      add_w :: Tup a -> PQueue a -> PQueue a -> PQueue a
      add_w v rest []        = rest ++ v:[]
      add_w v rest z@(x:xs)  | snd v <= snd x  = rest ++ v:z
                             | otherwise       = add_w v (rest++[x]) xs

prioritize :: (Eq a) => [a] -> PQueue a -> PQueue a
prioritize [] acc     = pqsort acc
prioritize (x:xs) acc | not . elem (Leaf x) . fst $ unzip acc = prioritize xs $ (Leaf x,1):acc
                      | otherwise = prioritize xs $ map (\w -> if fst w == (Leaf x) then (fst w, snd w + 1) else w) acc

pqsort :: PQueue a -> PQueue a
pqsort []      = []
pqsort (x:xs)  = pqsort left ++ [x] ++ pqsort right
   where
      left  = filter (\w -> snd w < snd x) xs
      right = filter (\w -> snd w >= snd x) xs

treeify :: PQueue a -> BTree a
treeify [x]       = fst x
treeify (x:y:xs)  = treeify $ add (combine x y) xs

combine :: Tup a -> Tup a -> Tup a
combine (b1,i1) (b2,i2) = ((Node b1 b2), i1+i2)

tablefy :: BTree a -> [(a,String)]
tablefy tree = tablefy_w tree ""
   where
      tablefy_w :: BTree a -> String -> [(a,String)]
      tablefy_w (Leaf a) path     = [(a,path)]
      tablefy_w (Node b1 b2) path = (tablefy_w b1 (path++"0")) ++ (tablefy_w b2 (path++"1"))

lookup2 :: (Eq a) => [(a,String)] -> a -> String
lookup2 [] _       = error "Lookup not found"
lookup2 (x:xs) a   | fst x == a   = snd x
                   | otherwise    = lookup2 xs a

encode :: (Eq a) => [a] -> (String,BTree a)
encode list = (extend . concat $ map (lookup2 huffmantable) list, huffmantree)
   where
      huffmantree  = treeify $ prioritize list []
      huffmantable = tablefy huffmantree
      extend xs = xs ++ (replicate (padding xs) '0') ++ (padtail $ padding xs)
      padding xs = ((8 - (length xs + 3) `mod` 8) `mod` 8)
      padtail xs  | xs == 0 = "000"
                  | xs == 1 = "001"
                  | xs == 2 = "010"
                  | xs == 3 = "011"
                  | xs == 4 = "100"
                  | xs == 5 = "101"
                  | xs == 6 = "110"
                  | otherwise = "111"

decode :: (String,BTree a) -> [a]
decode (str,tree) = decode_w tree $ get_2_bytes str
   where
      decode_w (Leaf v) (xs,str)          | and [xs == [], str == []] = [v]
                                          | otherwise = v:(decode_w tree (xs,str))
      decode_w (Node b1 b2) ((x:xs),str)  | x == '0'  = decode_w b1 (xs,str)
                                          | otherwise = decode_w b2 (xs,str)
      decode_w t ([],str)                 | str /= [] = decode_w t $ get_2_bytes str 
                                          | otherwise = []

get_2_bytes :: String -> (String,String)
get_2_bytes str | length str == 16  = (take (6 + (7 - bitdecode (drop 13 str))) str,drop 16 str)
                | length str == 8   = (take (8 - (3 + bitdecode (drop 5 str))) str, drop 8 str)
                | otherwise         = (take 16 str,drop 16 str)
   where
      bitdecode bit  | bit == "000" = 0
                     | bit == "001" = 1
                     | bit == "010" = 2
                     | bit == "011" = 3
                     | bit == "100" = 4
                     | bit == "101" = 5
                     | bit == "110" = 6
                     | otherwise    = 7

binary_string_to_word8 :: String -> Word8
binary_string_to_word8 s = h s 7 0
   where
      g :: Char -> Word8
      g c = if c == '0' then 0 else 1
      h :: String -> Word8 -> Word8 -> Word8
      h (x:xs) index acc   | index == 0   = 2^0*(g x) + acc
                           | otherwise    = h xs (index-1) $ 2^index * (g x) + acc

word8_to_binary_string :: Word8 -> String
word8_to_binary_string s = h s 7 ""
   where
      n :: Word8 -> Word8 -> (Word8,Char)
      n w d = if w - d > w then (w,'0') else (w - d,'1')
      h :: Word8 -> Word8 -> String -> String
      h w index acc | index == 0 = acc ++ [(snd (n w (2^index)))]
                    | otherwise  = h (fst (n w (2^index))) (index-1) (acc ++ [(snd (n w (2^index)))])

tobinary :: (Eq a) => [a] -> (BS.ByteString,BTree a)
tobinary = fsty BS.pack . fsty (map binary_string_to_word8) . fsty h1 . encode
   where
      h1 [] = []
      h1 xs = take 8 xs : (h1 $ drop 8 xs)

frombinary :: (BS.ByteString,BTree a) -> [a]
frombinary = decode . fsty concat . fsty (map word8_to_binary_string) . fsty BS.unpack

fsty :: (a -> b) -> (a,c) -> (b,c)
fsty f (a,c) = (f a,c)
