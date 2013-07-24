module Huffman.Decode (
    decode
)   where

import Huffman.Encode (BTree (..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char
import Data.Word8 hiding (isDigit)

decode :: BL.ByteString -> BTree a -> [a]
decode bytes btree  = readEncoding encoded_bits btree
    where
        unpacked_bytes  = BL.unpack bytes
        encoded_bits    = removePadding
                        . concat
                        $ map word8_to_bits unpacked_bytes

fromBinary :: (Read a) => BL.ByteString -> [a]
fromBinary byte_string  = decode encoding btree
    where
        tree_length = read 
                    . BLC.unpack
                    . BLC.takeWhile isDigit
                    $ byte_string
        btree       = read
                    . BLC.unpack
                    . BLC.take tree_length
                    . BLC.dropWhile isDigit
                    $ byte_string
        encoding    = BL.drop tree_length
                    . BLC.dropWhile isDigit
                    $ byte_string

-- | Read a list of bits and convert them into values using a given BTree.
readEncoding :: [Word8] -> BTree a -> [a]
readEncoding bits btree = readEncoding' bits btree
    where
        readEncoding' xs (Leaf v)           = v:readEncoding' xs btree
        readEncoding' [] _                  = []
        readEncoding' (x:xs) (Node t1 t2)   = if x == _0
                                                then readEncoding' xs t1
                                                else readEncoding' xs t2

removePadding :: [Word8] -> [Word8]
removePadding xs   = take (length xs - (3 + padding_decoded)) xs
    where
        decode_padding bits | bits == [_0,_0,_0]    = 0
                            | bits == [_0,_0,_1]    = 1
                            | bits == [_0,_1,_0]    = 2
                            | bits == [_0,_1,_1]    = 3
                            | bits == [_1,_0,_0]    = 4
                            | bits == [_1,_0,_1]    = 5
                            | bits == [_1,_1,_0]    = 6
                            | otherwise             = 7
        padding_decoded     = decode_padding
                            $ drop (length xs - 3) xs

-- | Turn a Word8 into eight Word8s that represent the 0 and 1 bits
-- | used to make it.
word8_to_bits :: Word8 -> [Word8]
word8_to_bits = helper 7
    where
        helper :: Word8 -> Word8 -> [Word8]
        helper index word   | index == 0    = [bit]
                            | otherwise     = bit : (helper (index - 1) new_word)
            where
                reduce                      = word - 2^index < word
                (bit, new_word) | reduce    = (_1,word - 2^index)
                                | otherwise = (_0,word)
        
