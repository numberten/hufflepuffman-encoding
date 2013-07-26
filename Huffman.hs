import qualified Data.ByteString.Lazy as BL
import System.Environment

import Huffman.Encode
import Huffman.Decode

main :: IO ()
main = getArgs >>= runArgs

runArgs :: [String] -> IO ()
runArgs (action:filepath:newfile:[])
            | action == "encode"
            = do
                file <- readFile filepath
                BL.writeFile newfile $ toBinary file
            | action == "decode"
            = do
                binary <- BL.readFile filepath
                writeFile newfile $ fromBinary binary
            | otherwise
            = error error_msg
runArgs _   = error error_msg
error_msg = "Invalid command line arguments.\n\
             \(encode|decode) \
             \<file_to_be_encoded/decoded> \
             \<new_file_name>"
                                      
