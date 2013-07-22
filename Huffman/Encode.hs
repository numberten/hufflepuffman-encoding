module Huffman.Encode (
    encode
,   BTree (..)
)   where

import qualified Data.PSQueue as PQ
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Map as M hiding (foldl')
import Data.Maybe
import Data.Word8
import Debug.Trace

data BTree a = Leaf a | Node (BTree a) (BTree a) deriving (Eq, Ord, Show, Read)
--type Tup a = (BTree a, Int)
--type PQueue a = [Tup a]

encode :: (Eq a, Ord a) => [a] -> (BL.ByteString,BTree a)
encode xs = (binary_encoding, huffmantree)
    where
    --  | Intermediate data structures.
        priority_queue  = makeQueue xs
        huffmantree     = fromJust $ makeBTree priority_queue
        huffmantable    = makeTable huffmantree
        encoding        = concat
                        . fmap (fromJust . flip M.lookup huffmantable)
                        $ xs
    --  | Padding the encoded values so they'll fit nicely into bytes.
        number_of_zeros = (8 - (length encoding + 3) `mod` 8) `mod` 8
        zeros           = replicate number_of_zeros _0
        padding         = zeros ++ padtail number_of_zeros
        padded_encoding = encoding ++ padding
    --  | Package the bits into a compressed ByteString.
        chunks []       = []
        chunks bits     = take 8 bits : (chunks $ drop 8 bits)
        binary_encoding = BL.pack
                        . fmap readBits 
                        $ chunks padded_encoding
        
        

makeQueue :: (Eq a, Ord a) => [a] -> PQ.PSQ (BTree a) Int
makeQueue xs = foldl' f PQ.empty xs
    where
        f :: (Eq a, Ord a) => PQ.PSQ (BTree a) Int -> a -> PQ.PSQ (BTree a) Int
        f acc value = PQ.insertWith (\p1 -> \p2 -> p1 + p2) (Leaf value) 1 acc

makeBTree :: (Eq a, Ord a) => PQ.PSQ (BTree a) Int -> Maybe (BTree a)
makeBTree queue | PQ.null queue         = Nothing
                | PQ.size queue == 1    = PQ.findMin queue
                                      >>= return . PQ.key
                | otherwise             = makeBTree
                                        $ PQ.insert new_tree new_priority remainder'
    where
        Just (lowest, remainder)    = PQ.minView queue
        Just (lowest', remainder')  = PQ.minView remainder
        new_priority                = PQ.prio lowest + PQ.prio lowest' 
        new_tree                    = Node (PQ.key lowest) (PQ.key lowest')
        
makeTable :: (Ord a) => BTree a -> M.Map a [Word8]
makeTable btree = makeTable' btree []
    where
        makeTable' :: (Ord a) => BTree a -> [Word8] -> M.Map a [Word8]
        makeTable' (Leaf a) path            = M.singleton a path
        makeTable' (Node left right) path   = makeTable' left (path ++ [_0])
                                              `M.union`
                                              makeTable' right (path ++ [_1])

-- | Last three bits of the padding represent a number between 0 and 7.
-- | This number tells `func` how many zeros to ignore before the padtail.
padtail 0 = [_0,_0,_0]
padtail 1 = [_0,_0,_1]
padtail 2 = [_0,_1,_0]
padtail 3 = [_0,_1,_1]
padtail 4 = [_1,_0,_0]
padtail 5 = [_1,_0,_1]
padtail 6 = [_1,_1,_0]
padtail 7 = [_1,_1,_1]

{--
encode :: (Eq a) => [a] -> (String,BTree a)
encode list = (extend . concat $ map (lookup2 huffmantable) list, huffmantree)
   where
      huffmantree  = treeify $ prioritize list []
      huffmantable = tablefy huffmantree
      extend xs = xs ++ (replicate (padding xs) '0') ++ (padtail $ padding xs)
      padding xs = ((8 - (length xs + 3) `mod` 8) `mod` 8)
      padtail i  | i == 0 = "000"
                 | i == 1 = "001"
                 | i == 2 = "010"
                 | i == 3 = "011"
                 | i == 4 = "100"
                 | i == 5 = "101"
                 | i == 6 = "110"
                 | otherwise = "111"

treeify :: PQueue a -> BTree a
treeify [x]       = fst x
treeify (x:y:xs)  = treeify $ add (combine x y) xs

prioritize :: (Eq a) => [a] -> PQueue a -> PQueue a
prioritize [] acc     = pqsort acc
prioritize (x:xs) acc | not . elem (Leaf x) . fst $ unzip acc = prioritize xs $ (Leaf x,1):acc
                      | otherwise = prioritize xs $ map (\w -> if fst w == (Leaf x) then (fst w, snd w + 1) else w) acc

lookup2 :: (Eq a) => [(a,String)] -> a -> String
lookup2 [] _       = error "Lookup not found"
lookup2 (x:xs) a   | fst x == a   = snd x
                   | otherwise    = lookup2 xs a

pqsort :: PQueue a -> PQueue a
pqsort []      = []
pqsort (x:xs)  = pqsort left ++ [x] ++ pqsort right
   where
      left  = filter (\w -> snd w < snd x) xs
      right = filter (\w -> snd w >= snd x) xs

combine :: Tup a -> Tup a -> Tup a
combine (b1,i1) (b2,i2) = ((Node b1 b2), i1+i2)

add :: Tup a -> PQueue a -> PQueue a
add a b = add_w a [] b
   where
      add_w :: Tup a -> PQueue a -> PQueue a -> PQueue a
      add_w v rest []        = rest ++ v:[]
      add_w v rest z@(x:xs)  | snd v <= snd x  = rest ++ v:z
                             | otherwise       = add_w v (rest++[x]) xs

tablefy :: BTree a -> [(a,String)]
tablefy tree = tablefy_w tree ""
   where
      tablefy_w :: BTree a -> String -> [(a,String)]
      tablefy_w (Leaf a) path     = [(a,path)]
      tablefy_w (Node b1 b2) path = (tablefy_w b1 (path++"0")) ++ (tablefy_w b2 (path++"1"))

tobinary :: (Eq a) => [a] -> (BL.ByteString,BTree a)
tobinary = fsty BL.pack . fsty (map binary_string_to_word8) . fsty h1 . encode
   where
      h1 [] = []
      h1 xs = take 8 xs : (h1 $ drop 8 xs)

fsty :: (a -> b) -> (a,c) -> (b,c)
fsty f (a,c) = (f a,c)

binary_string_to_word8 :: String -> Word8
binary_string_to_word8 s = h s 7 0
   where
      g :: Char -> Word8
      g c = if c == '0' then 0 else 1
      h :: String -> Word8 -> Word8 -> Word8
      h (x:xs) index acc   | index == 0   = 2^0*(g x) + acc
                           | otherwise    = h xs (index-1) $ 2^index * (g x) + acc

--}

readBits :: [Word8] -> Word8
readBits bits = trace ("Bits: " ++ show binaryBits) $ helper binaryBits 7 0
    where
        binaryBits              = fmap (\x -> if x == _0 then 0 else 1) bits
        helper :: [Word8] -> Word8 -> Word8 -> Word8
        helper (x:xs) index acc | index == 0    = 2^0*x + acc
                                | otherwise     = helper xs (index-1)
                                                $ 2^index * x + acc


