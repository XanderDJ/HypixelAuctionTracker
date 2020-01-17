{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import           Lib
import           Network.HTTP.Client     hiding ( host )
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status      ( statusCode )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as B
import           Data.Aeson              hiding ( Value
                                                , Array
                                                )
import           Data.List               hiding ( delete )
import           Data.Array
import           Data.Int
import           Data.Maybe
import           Data.Either                    ( fromRight )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Database.MongoDB        hiding ( find
                                                , group
                                                , sort
                                                , insert
                                                , Array
                                                )
import qualified Database.MongoDB              as DB
                                                ( find
                                                , group
                                                , sort
                                                , insert
                                                , Value(String)
                                                )
import qualified Data.NBT                      as N
import qualified Data.Serialize                as S
                                                ( decodeLazy )
import qualified Codec.Compression.GZip        as GZ
import qualified Data.ByteString.Base64.Lazy   as B64
                                                ( decode )
import           Control.Concurrent
import           Control.Retry
import           Control.Monad
--import used for robustness sake. If an api call (to db or to endpoint) fails we'll retry after a certain amount of time. Time will be calculated experimentally



main :: IO ()
main = do
    pipe <- connect $ host "127.0.0.1"
    mv <- newMVar []
    c1 <- newChan
    c2 <- newChan 
    forkIO $ apProducer c1 mv pipe
    forkIO $ replicateM_ 10 $ apConsumer c1 c2
    forkIO $ replicateM_ 10 $ agConsumer c2 mv pipe
    print "Threads created.\n"
    -- make main wait forever
    getLine
    print "Finished.\n"




-- DATA TYPES FOR JSON --

data AuctionPage =
    AuctionPage {
        succes :: Bool,
        page :: Int,
        totalPages :: Int,
        auctions :: [Auction]
    } deriving Show

data Auction =
    Auction {
        uuid :: Text,   -- Part of index for the updates
        startAuction :: Int64, -- Part of index for the updates
        estimatedEnd :: Int64,
        itemName :: Text,
        itemLore :: Text,
        reforge :: Text,
        enchants :: [Text],
        hpb ::Int,
        aAmount :: Int,
        extra :: Text,
        category :: Text,
        tier :: Text,
        nbt :: N.NBT,
        startingBid :: Int,
        claimed :: Bool,
        highestBidAmount :: Int,
        bids :: [Bid]
    } deriving Show

data AuctionGroup = AuctionGroup {
    gItemName :: Text,
    gAuctions :: [Auction]
} deriving Show

data Bid =
    Bid {
        auctionId :: Text,
        amount :: Int,
        ts :: Int64
    } deriving Show



-- Typeclasses --

class ToBSON a where
    toBSON :: a -> Document

-- Instances -- 

instance Eq Auction where
    x == y = itemName x == itemName y

instance Ord Auction where
    compare x y = compare (itemName x) (itemName y)


instance FromJSON AuctionPage where
    parseJSON (Object jsn) = do
        succes       <- jsn .: "success"
        page         <- jsn .: "page"
        totalPages   <- jsn .: "totalPages"
        auctions     <- jsn .: "auctions"
        auctionsList <- mapM parseJSON auctions
        return $ AuctionPage succes page totalPages auctionsList

instance FromJSON Auction where
    parseJSON (Object jsn) = do
        uuid             <- jsn .: "uuid"
        startAuction     <- jsn .: "start"
        estimatedEnd     <- jsn .: "end"
        itemName         <- jsn .: "item_name"
        itemLore         <- jsn .: "item_lore"
        extra            <- jsn .: "extra"
        category         <- jsn .: "category"
        tier             <- jsn .: "tier"
        bytestring       <- jsn .: "item_bytes"
        startingBid      <- jsn .: "starting_bid"
        claimed          <- jsn .: "claimed"
        highestBidAmount <- jsn .: "highest_bid_amount"
        bids             <- jsn .: "bids"
        bidsList         <- mapM parseJSON bids
        return $ Auction
            uuid
            (fromIntegral startAuction)
            (fromIntegral estimatedEnd)
            itemName
            itemLore
            "None"
            ["None"]
            0
            1
            extra
            category
            tier
            ((bsToNBT . fromRight "" . B64.decode . encodeUtf8) bytestring)
            startingBid
            claimed
            highestBidAmount
            bidsList


instance FromJSON Bid where
    parseJSON (Object jsn) = do
        auctionId <- jsn .: "auction_id"
        amount    <- jsn .: "amount"
        timestamp <- jsn .: "timestamp"
        return $ Bid { auctionId = auctionId
                     , amount    = amount
                     , ts        = fromIntegral timestamp
                     }


instance ToBSON AuctionGroup where
    toBSON ahGroup =
        let ah    = head . gAuctions $ ahGroup
            cat   = category ah
            tier' = tier ah
        in  [ "itemGroup" =: gItemName ahGroup
            , "category" =: cat
            , "tier" =: tier'
            ]

instance ToBSON Auction where
    toBSON ah =
        [ "uuid" =: uuid ah
        , "start" =: (MongoStamp . startAuction) ah
        , "end" =: (MongoStamp . estimatedEnd) ah
        , "reforge" =: reforge ah
        , "hpb" =: hpb ah
        , "amount" =: aAmount ah
        , "enchants" =: enchants ah
        , "startBid" =: startingBid ah
        , "highestBid" =: highestBidAmount ah
        , "bids" =: map toBSON (bids ah)
        ]

instance ToBSON Bid where
    toBSON bid = ["amount" =: amount bid, "timestamp" =: (MongoStamp . ts) bid]

-- Functions

-- IO (Database and API requests)
restDelayPolicy :: RetryPolicy
restDelayPolicy = constantDelay 1000000 <> limitRetries 5

apProducer :: Chan Int -> MVar [Document] -> Pipe -> IO ()
apProducer c mv p = forever $ do
    print "Getting auction page 0"
    dl <- takeMVar mv
    let runDb = access p master "HypixelAH"
    allItems <- runDb getAllItems
    putMVar mv allItems
    let f _ = getAuctionPage 0
    print "Got auction page 0"
    -- retry 5 times to get the auction page. with a delay of 1 second
    ap0 <- retrying restDelayPolicy (const $ return . isNothing) f
    if isNothing ap0
        then do 
            print "Going to sleep for 30 seconds"
            sleepS 30 -- If retry policy failed sleep for 30 seconds and try again
        else do
            let pages = [0 .. (totalPages . fromJust $ ap0)]
            writeList2Chan c pages
            print "going to sleep for 60 seconds"
            sleepS 60 -- If it succeeded wait for 60 seconds before getting the next batch of pages that need to be read (max calls per min is 120)

apConsumer :: Chan Int -> Chan AuctionGroup -> IO ()
apConsumer consumeChan produceChan = forever $ do
    n <- readChan consumeChan
    let f _ = getAuctionPage n -- I don't care about retry status. If it fails it fails
    ap <- retrying restDelayPolicy (const $ return . isNothing) f
    if isNothing ap
        then writeChan consumeChan n --If we couldn't get page n then put n back onto fifo chan
        else do
            let
                ags =
                    ( convertToAuctionGroup
                        . getGroupedAuctions
                        . updateAuctionPage
                        . fromJust
                        )
                        ap
            writeList2Chan produceChan ags
    sleepS 1 -- Sleep 1 sec to definitely make sure that the api limit isn't crossed

agConsumer :: Chan AuctionGroup -> MVar [Document] -> Pipe -> IO ()
agConsumer c mv p = forever $ do
    ag <- readChan c
    let runDb = access p master "HypixelAH"
        sAG _ = (runDb . storeAuctionGroup) ag
        sAGM _ = (runDb . storeAuctionGroupMeta) ag
    -- Try 5 times to store the auction group with 50 ms in between tries
    retrying retryPolicyDefault (const $ return . failed) sAG
    dl <- takeMVar mv
    putMVar mv dl
    let inDb = txtInDL "itemGroup" dl . gItemName
    if inDb ag then return () else void $ (runDb . storeAuctionGroupMeta) ag

getCollections :: Pipe -> IO [Collection]
getCollections pipe = access pipe master "HypixelAH" allCollections

-- Stores the item, category and tier of the AuctionGroup provided. Should only be called if the item group is not in the items collection yet
storeAuctionGroupMeta :: AuctionGroup -> Action IO ()
storeAuctionGroupMeta = insert_ "items" . toBSON


storeAuctionGroup :: AuctionGroup -> Action IO WriteResult
storeAuctionGroup ag = updateAll (gItemName ag) (map ahToUp (gAuctions ag))



getAllItems :: Action IO [Document]
getAllItems = rest =<< DB.find (select [] "items")
    { project = ["itemGroup" =: 1, "_id" =: 0]
    }


-- | get api key to use for api.hypixel.net from file in ./apikey/HypixelApiKey.txt
getHypixelApiKey :: IO String
getHypixelApiKey = readFile "apikey/HypixelApiKey.txt"

-- | Get the current auctions. Paginated
getAuctionPage :: Int -> IO (Maybe AuctionPage)
getAuctionPage number = do
    apiKey  <- getHypixelApiKey
    manager <- newManager tlsManagerSettings
    request <-
        parseRequest
        $  "https://api.hypixel.net/skyblock/auctions?page="
        ++ (show number)
        ++ "&key="
        ++ apiKey
    response <- httpLbs request manager
    return (decode (responseBody response) :: Maybe AuctionPage)


getKeyUsages :: IO (Response B.ByteString)
getKeyUsages = do
    apiKey  <- getHypixelApiKey
    manager <- newManager tlsManagerSettings
    request <- parseRequest $ "https://api.hypixel.net/key?=key=" ++ apiKey
    httpLbs request manager


-- PURE

-- NBT RELATED STUFF
bsToNBT :: B.ByteString -> N.NBT
bsToNBT bs = nbt
  where
    eitherNBT = (S.decodeLazy . GZ.decompress) bs :: Either String N.NBT
    getNBTFromEither (Left  s  ) = N.NBT "Invalid" (N.StringTag "Invalid")
    getNBTFromEither (Right nbt) = nbt
    nbt = getNBTFromEither eitherNBT

getNBTAttr :: Text -> N.NBT -> Maybe N.NbtContents
getNBTAttr attr (N.NBT name (N.ByteTag n)) =
    if name == attr then Just $ N.ByteTag n else Nothing

getNBTAttr attr (N.NBT name (N.ShortTag n)) =
    if name == attr then Just $ N.ShortTag n else Nothing

getNBTAttr attr (N.NBT name (N.IntTag n)) =
    if name == attr then Just $ N.IntTag n else Nothing

getNBTAttr attr (N.NBT name (N.LongTag n)) =
    if name == attr then Just $ N.LongTag n else Nothing

getNBTAttr attr (N.NBT name (N.FloatTag n)) =
    if name == attr then Just $ N.FloatTag n else Nothing

getNBTAttr attr (N.NBT name (N.DoubleTag n)) =
    if name == attr then Just $ N.DoubleTag n else Nothing

getNBTAttr attr (N.NBT name (N.ByteArrayTag arr)) =
    if name == attr then Just $ N.ByteArrayTag arr else Nothing

getNBTAttr attr (N.NBT name (N.StringTag txt)) =
    if name == attr then Just $ N.StringTag txt else Nothing

getNBTAttr attr (N.NBT name (N.IntArrayTag arr)) =
    if name == attr then Just $ N.IntArrayTag arr else Nothing

getNBTAttr attr (N.NBT name (N.LongArrayTag arr)) =
    if name == attr then Just $ N.LongArrayTag arr else Nothing

getNBTAttr attr (N.NBT name (N.ListTag arr)) = if name == attr
    then Just $ N.ListTag arr
    else go ems
  where
    ems = elems arr
    go :: [N.NbtContents] -> Maybe N.NbtContents
    go [] = Nothing
    go ems =
        let nbcs = mapMaybe (getNBTAttr attr . N.NBT "") ems
        in  if length nbcs == 0 then Nothing else Just $ head nbcs

getNBTAttr attr (N.NBT name (N.CompoundTag list)) = if name == attr
    then Just $ N.CompoundTag list
    else go list
  where
    go :: [N.NBT] -> Maybe N.NbtContents
    go [] = Nothing
    go lst =
        let mNC = mapMaybe (getNBTAttr attr) lst
        in  if length mNC == 0 then Nothing else Just $ head mNC

sleepMs :: Int -> IO ()
sleepMs = threadDelay . (*) 1000

sleepS = sleepMs . (*) 1000

-- REST

ahToUp :: Auction -> (Selector, Document, [UpdateOption])
ahToUp ah =
    ( ["uuid" =: uuid ah, "start" =: (MongoStamp . startAuction) ah]
    , toBSON ah
    , [Upsert]
    )

txtInDL :: Label -> [Document] -> Text -> Bool
txtInDL label docs txt = elem (DB.String txt) (map (valueAt label) docs)

updateAuctionPage :: AuctionPage -> AuctionPage
updateAuctionPage ap = ap { auctions = newAuctions }
  where
    newAuctions =
        map (addReforge . addEnchants . addHpb . addAmount) $ auctions ap

getGroupedAuctions :: AuctionPage -> [[Auction]]
getGroupedAuctions = group . quicksort . auctions

convertToAuctionGroup :: [[Auction]] -> [AuctionGroup]
convertToAuctionGroup groupedAuctions =
    [ AuctionGroup { gItemName = newItemName auctionGroup
                   , gAuctions = auctionGroup
                   }
    | auctionGroup <- groupedAuctions
    ]
    where newItemName = itemName . head

quicksort :: Ord a => [a] -> [a]
quicksort []       = []
quicksort (p : xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser  = filter (< p) xs
    greater = filter (>= p) xs

addAmount :: Auction -> Auction
addAmount ah = ah { aAmount = newAmount }
  where
    count     = getAmount $ nbt ah
    newAmount = maybe 1 getInt count

getAmount :: N.NBT -> Maybe N.NbtContents
getAmount = getNBTAttr "Count"

-- There are a couple of weird things. Some items can have a reforge that has the same name as the prefix of the item. I.e. superior and superior dragon boots. This will then be transformed into very
-- But in the NBT info it will still say the superior as the reforge and not very. Same goes for strong and wise. This causes names to be shortened in the wrong way
addReforge :: Auction -> Auction
addReforge ah = ah { reforge = mod', itemName = newItemName }
  where
    modifier    = getReforge $ nbt ah
    reforgeName = maybe (reforge ah) getStringTag modifier
    mod'        = if reforgeName `elem` weirdMods
        then fst $ T.span (\x -> x `notElem` "_") reforgeName
        else reforgeName
    newItemName = if isNothing modifier
        then itemName ah
        else getItemNameWithoutMod $ itemName ah

getItemNameWithoutMod :: Text -> Text
getItemNameWithoutMod txt = itemName
    where itemName = (dropN 1 . snd) $ T.span (\x -> x `notElem` " ") txt

weirdMods :: [Text]
weirdMods = ["rich_sword", "odd_sword", "rich_bow", "odd_bow"]

getReforge :: N.NBT -> Maybe N.NbtContents
getReforge = getNBTAttr "modifier"

getStringTag :: N.NbtContents -> Text
getStringTag (N.StringTag txt) = txt

getInt :: N.NbtContents -> Int
getInt (N.ByteTag  n) = fromIntegral n
getInt (N.ShortTag n) = fromIntegral n
getInt (N.IntTag   n) = fromIntegral n
getInt (N.LongTag  n) = fromIntegral n
getInt _              = 0

addEnchants :: Auction -> Auction
addEnchants ah = ah { enchants = newEnchants }
  where
    enchant'    = getEnchantsFromNbt $ nbt ah
    newEnchants = maybe [] getEnchants enchant'

getEnchants :: N.NbtContents -> [Text]
getEnchants (N.CompoundTag nbts) = [ nbtEnchantToText nbt | nbt <- nbts ]
  where
    nbtEnchantToText :: N.NBT -> Text
    nbtEnchantToText (N.NBT enchantName (N.IntTag n)) =
        T.pack $ (T.unpack enchantName) ++ " " ++ (intToRoman . fromIntegral) n

getEnchantsFromNbt :: N.NBT -> Maybe N.NbtContents
getEnchantsFromNbt = getNBTAttr "enchantments"


addHpb :: Auction -> Auction
addHpb ah = ah { hpb = newHpb }
  where
    hpbAmount = getHpb $ nbt ah
    newHpb    = maybe 0 getInt hpbAmount

getHpb :: N.NBT -> Maybe N.NbtContents
getHpb = getNBTAttr "hot_potato_count"

dropN :: Int -> Text -> Text
dropN n ""   = T.empty
dropN 0 text = text
dropN n text = dropN (n - 1) $ T.tail text

convMap = [(10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

intToRoman :: Int -> String
intToRoman = concat . unfoldr findLeast
  where
    findLeast n = case i of
        Just (x, r) -> Just (r, n - x)
        Nothing     -> Nothing
        where i = find (\(val, _) -> val <= n) convMap
