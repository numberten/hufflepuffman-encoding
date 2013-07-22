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

data BTree a = Leaf a | Node (BTree a) (BTree a) deriving (Eq, Ord, Show, Read)

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
-- | This number tells the decoding function how many zeros to ignore
-- | before the padtail.
padtail 0 = [_0,_0,_0]
padtail 1 = [_0,_0,_1]
padtail 2 = [_0,_1,_0]
padtail 3 = [_0,_1,_1]
padtail 4 = [_1,_0,_0]
padtail 5 = [_1,_0,_1]
padtail 6 = [_1,_1,_0]
padtail 7 = [_1,_1,_1]

-- | Turns a length eight list of Word8s, all of which are either zeros
-- | or ones, into a single Word8.
readBits :: [Word8] -> Word8
readBits bits = helper binaryBits 7 0
    where
        binaryBits              = fmap (\x -> if x == _0 then 0 else 1) bits
        helper :: [Word8] -> Word8 -> Word8 -> Word8
        helper (x:xs) index acc | index == 0    = 2^0*x + acc
                                | otherwise     = helper xs (index-1)
                                                $ 2^index * x + acc


