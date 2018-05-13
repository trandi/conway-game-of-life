module Peggy2Board (drawBool) where

import System.RaspberryPi.GPIO
import qualified Data.ByteString as BS
import Data.Word
import Data.List.Split
import Control.Exception
import GHC.IO.Exception

bAddress :: Address
bAddress = 0x22

bSize = 25

-- standard header is 0xdeadbeef (sent in big endian order)
-- followed by a 1 and 0 (reserved values)
bHeader :: [Word8]
bHeader = [0xde, 0xad, 0xbe, 0xef, 1, 0]


drawBool :: [Bool] -> IO ()
drawBool bs = draw $ map (\b -> if b then 15 else 0) bs



-- the input should be the list of brightnesses of values 0-15, of size bSize * bSize = 625 exactly !
draw :: [Word8] -> IO ()
draw brightnesses = withGPIO . withI2C $ do
    writeI2C bAddress (BS.pack $ bHeader ++ (encodeBrightnesses brightnesses))
    `catch` \e -> do 
        putStrLn ("Caught " ++ show (e :: SomeException) ++ ". Trying again...")


encodeBrightnesses :: [Word8] -> [Word8]
encodeBrightnesses bs = 
    concat encodedRows
    where
        rows = chunksOf bSize bs
        encodedRows = map encodeRow rows

-- takes 4 bits brightness values and encodes them as 8 bits values.
-- the 1st column has the first 4 bits, the 2nd the bits 5-8. This wasy only 1 byte for 2 columns/values is used.
encodeRow :: [Word8] -> [Word8]
encodeRow bs = 
    -- shifting the 2nd elems of pairs by 4 bits to the left
    map (\(x, y) -> x + y * 16) paired
    where
        paired = pairEvensOddsWithDefault 0 bs




pairEvensOddsWithDefault :: a -> [a] -> [(a, a)]
pairEvensOddsWithDefault defaultValue input =
    -- evens sub list can be 1 element longer
    take (length eInput) pairs
    where
        eInput = evens input
        oInput = odds input
        pairs = zip eInput (oInput ++ (repeat defaultValue))

evens :: [a] -> [a]
evens (head: tail) = head: odds tail
evens _ = []

odds :: [a] -> [a]
odds (_: tail) = evens tail
odds _ = []
