import Numeric.Natural (Natural)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, liftEither)
import Control.Monad.State (State, MonadState (get), modify, runState, replicateM, replicateM_)
import Data.Bool (bool)
type Nat = Natural

type Bits = [Bool]
data Packet = Literal Nat Nat | Operator Nat Nat [Packet]
    deriving Show

getBits :: Nat -> ExceptT String (State Bits) Bits
getBits n = do
    str <- get
    let result = take (fromIntegral n) str
    modify $ drop $ fromIntegral n
    if length result < fromIntegral n
      then throwError "End of input"
      else pure result

bitsToNat :: [Bool] -> Nat
bitsToNat = go . reverse
  where
    go :: [Bool] -> Nat
    go [] = 0
    go (True : xs) = 1 + 2 * go xs
    go (False : xs) =  2 * go xs

parseDigitGroups :: ExceptT String (State Bits) Bits
parseDigitGroups = do
    continue <- getBits 1
    bits <- getBits 4
    if all (== True) continue
        then (bits ++) <$> parseDigitGroups
        else pure bits

manyPackets :: Bits -> Either String [Packet]
manyPackets bits =
    let (packet, str) = runState (runExceptT parsePacket) bits
    in case packet of
        Left e -> Left e
        Right p -> if null str
            then Right $ pure p
            else (p :) <$> manyPackets str

parsePacket :: ExceptT String (State Bits) Packet
parsePacket = do
    version <- bitsToNat <$> getBits 3
    packetType <- bitsToNat <$> getBits 3
    if packetType == 4
       -- Literal value
       then Literal version . bitsToNat <$> parseDigitGroups
       else do -- Operator
          lengthTypeId <- getBits 1
          if bitsToNat lengthTypeId == 0
            then do
                size <- getBits 15
                subPacketsBits <- getBits $ bitsToNat size
                subPackets <- liftEither $ manyPackets subPacketsBits
                pure $ Operator version packetType subPackets
            else do
                subPacketCount <- bitsToNat <$> getBits 11
                subPackets <- replicateM (fromIntegral subPacketCount) parsePacket
                pure $ Operator version packetType subPackets

fromHex :: String -> Bits
fromHex = concatMap fromHexChar
  where
    fromHexChar :: Char -> Bits
    fromHexChar '0' = map (== 1) [0, 0, 0, 0]
    fromHexChar '1' = map (== 1) [0, 0, 0, 1]
    fromHexChar '2' = map (== 1) [0, 0, 1, 0]
    fromHexChar '3' = map (== 1) [0, 0, 1, 1]
    fromHexChar '4' = map (== 1) [0, 1, 0, 0]
    fromHexChar '5' = map (== 1) [0, 1, 0, 1]
    fromHexChar '6' = map (== 1) [0, 1, 1, 0]
    fromHexChar '7' = map (== 1) [0, 1, 1, 1]
    fromHexChar '8' = map (== 1) [1, 0, 0, 0]
    fromHexChar '9' = map (== 1) [1, 0, 0, 1]
    fromHexChar 'A' = map (== 1) [1, 0, 1, 0]
    fromHexChar 'B' = map (== 1) [1, 0, 1, 1]
    fromHexChar 'C' = map (== 1) [1, 1, 0, 0]
    fromHexChar 'D' = map (== 1) [1, 1, 0, 1]
    fromHexChar 'E' = map (== 1) [1, 1, 1, 0]
    fromHexChar 'F' = map (== 1) [1, 1, 1, 1]
    fromHexChar c = error $ "Unrecognized hex char: " ++ [c]

versionNumberSum :: Packet -> Nat
versionNumberSum (Literal v _) = v
versionNumberSum (Operator v _ subPackets) =
    v + sum (map versionNumberSum subPackets)

boolToNat :: Bool -> Nat
boolToNat True = 1
boolToNat False = 0

packetValue :: Packet -> Nat
packetValue (Literal _ value) = value
packetValue (Operator _ 0 ps) = sum $ map packetValue ps
packetValue (Operator _ 1 ps) = product $ map packetValue ps
packetValue (Operator _ 2 ps) = minimum $ map packetValue ps
packetValue (Operator _ 3 ps) = maximum $ map packetValue ps
packetValue (Operator _ 5 [a, b]) = boolToNat $
    packetValue a > packetValue b
packetValue (Operator _ 6 [a, b]) = boolToNat $
    packetValue a < packetValue b
packetValue (Operator _ 7 [a, b]) = boolToNat $
    packetValue a == packetValue b
packetValue _ = error "Invalid packet"

main :: IO ()
main = do
    input' <- readFile "input/16.txt"
    let input = reverse $ drop 1 $ reverse input'
        bits = fromHex input
        (result, end) = runState (runExceptT parsePacket) bits

    case result of
        Left e -> print e
        -- Right p -> print $ versionNumberSum p
        Right p -> print $ packetValue p

    putStrLn $ "Left over: " ++ map (bool '0' '1') end
