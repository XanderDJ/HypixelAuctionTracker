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
import           Data.List
import           Data.Array
import           Data.Char
import           Data.Int
import           Data.Maybe
import           Data.Either                    ( fromRight )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Database.MongoDB        hiding ( find
                                                , group
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
    p <- connect $ host "127.0.0.1"
    mv   <- newMVar []
    c1   <- newChan
    c2   <- newChan
    forkIO $ apProducer c1 mv p
    forkIO $ apConsumer c1 c2
    forkIO $ replicateM_ 5 $ agConsumer c2 mv p
    print "Threads created.\n"
    -- make main wait forever and allow command line args to check on status
    commandLine c1 c2

commandLine :: Chan Int -> Chan AuctionGroup -> IO ()
commandLine cAp cAg = do
    req <- getLine
    case (T.toLower . T.pack) req of
        "key" -> do
            printKeyStatus
            commandLine cAp cAg
        "quit" -> putStrLn "quitting program, aborting all working threads"
        _      -> commandLine cAp cAg

printKeyStatus :: IO ()
printKeyStatus = do
    response <- responseApiKey
    print $ getKeyPage response


test = do
    response <- responseAuctionPage 0
    let auctionPage = (fromJust . getAuctionPage) response
    print auctionPage

-- DATA TYPES FOR JSON --
data KeyPage = KeyPage Bool KeyStatus

data KeyStatus = KeyStatus {
    key :: Text,
    totalQueries :: Int,
    apiRate :: Int
}

data AuctionGroup = AuctionGroup {
    gItemName :: Text,
    gAuctions :: [Auction]
}

data AuctionPage =
    AuctionPage {
        totalPages :: Int,
        auctions :: [Auction]
    }

data Auction =
    Auction {
        uuid :: Text,   -- Part of index for the updates
        startAuction :: Int64, -- Part of index for the updates
        estimatedEnd :: Int64,
        itemName :: Text,
        reforge :: Text,
        enchants :: [Text],
        hpb ::Int,
        aAmount :: Int,
        category :: Text,
        tier :: Text,
        startingBid :: Int,
        highestBidAmount :: Int,
        bids :: [Bid]
    }

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

instance Show AuctionGroup where
    show ag = (T.unpack . gItemName) ag ++ " with " ++ (show . length  . gAuctions) ag ++ " auctions"

instance Show AuctionPage where
    show ap = intercalate "\n" auctions'
        where auctions' = map show (auctions ap)

instance Show Auction where
    show a = intercalate " : " strs
      where
        txts = [sanitize (itemName a), reforge a, category a]
        strs = map T.unpack txts

instance Show KeyPage where
    show (KeyPage success status) = show status

instance Show KeyStatus where
    show ks =
        "Key "
            ++ key'
            ++ " has "
            ++ show total
            ++ " queries and has a current rate of "
            ++ show rate
            ++ " queries per minute"
      where
        key'  = (T.unpack . key) ks
        total = totalQueries ks
        rate  = apiRate ks



instance Eq Auction where
    x == y = itemName x == itemName y

instance Ord Auction where
    compare x y = compare (itemName x) (itemName y)

instance FromJSON KeyPage where
    parseJSON (Object jsn) = do
        success   <- jsn .: "success"
        record    <- jsn .: "record"
        keyStatus <- parseJSON record
        return $ KeyPage success keyStatus

instance FromJSON KeyStatus where
    parseJSON (Object jsn) = do
        queries <- jsn .: "totalQueries"
        key     <- jsn .: "key"
        rateM   <- jsn .:? "queriesInPastMin"
        let rate = fromMaybe 0 rateM
        return $ KeyStatus key queries rate


instance FromJSON AuctionPage where
    parseJSON (Object jsn) = do
        totalPages   <- jsn .: "totalPages"
        auctions     <- jsn .: "auctions"
        auctionsList <- mapM parseJSON auctions
        return $ AuctionPage totalPages auctionsList

instance FromJSON Auction where
    parseJSON (Object jsn) = do
        uuid             <- jsn .: "uuid"
        startAuction     <- jsn .: "start"
        estimatedEnd     <- jsn .: "end"
        itemName         <- jsn .: "item_name"
        category         <- jsn .: "category"
        tier             <- jsn .: "tier"
        bytestring       <- jsn .: "item_bytes"
        startingBid      <- jsn .: "starting_bid"
        highestBidAmount <- jsn .: "highest_bid_amount"
        bids             <- jsn .: "bids"
        bidsList         <- mapM parseJSON bids
        let nbt = (bsToNBT . fromRight "" . B64.decode . encodeUtf8) bytestring
            am          = getAmount nbt
            count       = maybe 1 getInt am
            reforge     = getReforge nbt
            properName  = getProperItemName itemName reforge
            enchant'    = getEnchantsFromNbt nbt
            newEnchants = maybe ["None"] getEnchants enchant'
            hpbAmount   = getHpb nbt
            newHpb      = maybe 0 getInt hpbAmount
        return $ Auction uuid
                         (fromIntegral startAuction)
                         (fromIntegral estimatedEnd)
                         properName
                         reforge
                         newEnchants
                         newHpb
                         count
                         category
                         tier
                         startingBid
                         highestBidAmount
                         bidsList


instance FromJSON Bid where
    parseJSON (Object jsn) = do
        auctionId <- jsn .: "auction_id"
        amount    <- jsn .: "amount"
        timestamp <- jsn .: "timestamp"
        return Bid { auctionId = auctionId
                   , amount    = amount
                   , ts        = fromIntegral timestamp
                   }


instance ToBSON AuctionGroup where
    toBSON ahGroup =
        let ah    = head . gAuctions $ ahGroup
            cat   = category ah
            tier' = tier ah
        in  [ "itemGroup" =: (sanitize . gItemName) ahGroup
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
apProducer :: Chan Int -> MVar [Document]-> Pipe -> IO ()
apProducer c mv p = forever $ do
    print "Getting auction page 0"
    dl <- takeMVar mv
    let runDb = access p master "HypixelAH"
    allItems <- runDb getAllItems
    putMVar mv allItems
    response <- responseAuctionPage 0
    print "Got auction page 0"
    let ap0 = getAuctionPage response
    if isNothing ap0
        then do
            print "Going to sleep for 30 seconds"
            sleepS 30
        else do
            let pages = [0 .. ((+) (-1) . totalPages . fromJust $ ap0)]
                seconds = 1800
            writeList2Chan c pages
            print $ "going to sleep for " ++ show seconds ++ " seconds"
            sleepS seconds -- If it succeeded wait for 60 seconds before getting the next batch of pages that need to be read (max calls per min is 120)

apConsumer :: Chan Int -> Chan AuctionGroup -> IO ()
apConsumer consumeChan produceChan = forever $ do
    n        <- getChanContents consumeChan
    responses <- mapM responseAuctionPage n -- Find a way to handle the Exceptions possibly caused by https client
    let aps = map (fromJust . getAuctionPage) responses -- throws error when a page fails
        as = concatMap auctions aps
        ags = (convertToAuctionGroup . getGroupedAuctions) as
    writeList2Chan produceChan ags

agConsumer :: Chan AuctionGroup -> MVar [Document] -> Pipe ->  IO ()
agConsumer c mv p = forever $ do
    ag <- readChan c
    let runDb = access p master "HypixelAH"
        sAG _ = (runDb . storeAuctionGroup) ag
    -- Try 5 times to store the auction group with 50 ms in between tries
    retrying retryPolicyDefault (const $ return . failed) sAG
    putStrLn $ "AGC : " ++ show ag
    dl <- takeMVar mv
    putMVar mv dl
    let inDb = txtInDL "itemGroup" dl . gItemName
    if inDb ag then return () else void $ (runDb . storeAuctionGroupMeta) ag

-- Stores the item, category and tier of the AuctionGroup provided. Should only be called if the item group is not in the items collection yet
storeAuctionGroupMeta :: AuctionGroup -> Action IO ()
storeAuctionGroupMeta = insert_ "items" . toBSON


storeAuctionGroup :: AuctionGroup -> Action IO WriteResult
storeAuctionGroup ag =
    updateAll ((sanitize . gItemName) ag) (map ahToUp (gAuctions ag))

ahToUp :: Auction -> (Selector, Document, [UpdateOption])
ahToUp ah =
    ( ["uuid" =: uuid ah, "start" =: (MongoStamp . startAuction) ah]
    , toBSON ah
    , [Upsert]
    )

getAllItems :: Action IO [Document]
getAllItems = rest =<< DB.find (select [] "items")
    { project = ["itemGroup" =: 1, "_id" =: 0]
    }


-- | get api key to use for api.hypixel.net from file in ./apikey/HypixelApiKey.txt
getHypixelApiKey :: IO String
getHypixelApiKey = readFile "apikey/HypixelApiKey.txt"


responseAuctionPage :: Int -> IO (Response B.ByteString)
responseAuctionPage n = do
    apiKey  <- getHypixelApiKey
    manager <- newManager tlsManagerSettings
    request <-
        parseRequest
        $  "https://api.hypixel.net/skyblock/auctions?page="
        ++ show n
        ++ "&key="
        ++ apiKey
    httpLbs request manager

responseApiKey :: IO (Response B.ByteString)
responseApiKey = do
    apikey  <- getHypixelApiKey
    manager <- newManager tlsManagerSettings
    request <- parseRequest $ "https://api.hypixel.net/key?key=" ++ apikey
    httpLbs request manager

showKeyUsage = do
    response <- responseApiKey
    let keyPage = fromJust $ getKeyPage response
    print keyPage

-- PURE

-- Sanitize names for database collections and stuff
sanitize :: Text -> Text
sanitize = changeSpaces . T.strip . T.filter f . T.toLower
  where
    f c = isSpace c || isLower c || isDigit c
    changeSpaces = T.replace " " "_"


-- | Get the current auctions from the response . Paginated
getAuctionPage :: Response B.ByteString -> Maybe AuctionPage
getAuctionPage = decode . responseBody

-- | Get the current key status from the response
getKeyPage :: Response B.ByteString -> Maybe KeyPage
getKeyPage = decode . responseBody

-- NBT RELATED STUFF
bsToNBT :: B.ByteString -> N.NBT
bsToNBT bs = nbt
  where
    eitherNBT = (S.decodeLazy . GZ.decompress) bs :: Either String N.NBT
    getNBTFromEither (Left  s  ) = N.NBT "Invalid" (N.StringTag "Invalid")
    getNBTFromEither (Right nbt) = nbt
    nbt = getNBTFromEither eitherNBT

getNBTAttr :: Text -> N.NBT -> Maybe N.NbtContents
getNBTAttr attr (N.NBT name tag) = case tag of
    (N.ByteTag   n) -> if name == attr then Just $ N.ByteTag n else Nothing
    (N.ShortTag  n) -> if name == attr then Just $ N.ShortTag n else Nothing
    (N.IntTag    n) -> if name == attr then Just $ N.IntTag n else Nothing
    (N.LongTag   n) -> if name == attr then Just $ N.LongTag n else Nothing
    (N.FloatTag  n) -> if name == attr then Just $ N.FloatTag n else Nothing
    (N.DoubleTag n) -> if name == attr then Just $ N.DoubleTag n else Nothing
    (N.ByteArrayTag arr) ->
        if name == attr then Just $ N.ByteArrayTag arr else Nothing
    (N.StringTag txt) ->
        if name == attr then Just $ N.StringTag txt else Nothing
    (N.IntArrayTag arr) ->
        if name == attr then Just $ N.IntArrayTag arr else Nothing
    (N.LongArrayTag arr) ->
        if name == attr then Just $ N.LongArrayTag arr else Nothing
    (N.ListTag arr) -> if name == attr then Just $ N.ListTag arr else goL arr
    (N.CompoundTag nbts) ->
        if name == attr then Just $ N.CompoundTag nbts else goC nbts
  where
    goL = goLEms . elems
    goLEms :: [N.NbtContents] -> Maybe N.NbtContents
    goLEms [] = Nothing
    goLEms ems =
        let nbcs = mapMaybe (getNBTAttr attr . N.NBT "") ems
        in  if null nbcs then Nothing else Just $ head nbcs
    goC :: [N.NBT] -> Maybe N.NbtContents
    goC [] = Nothing
    goC lst =
        let mNC = mapMaybe (getNBTAttr attr) lst
        in  if null mNC then Nothing else Just $ head mNC

sleepMs :: Int -> IO ()
sleepMs = threadDelay . (*) 1000

sleepS = sleepMs . (*) 1000

-- REST


txtInDL :: Label -> [Document] -> Text -> Bool
txtInDL label docs txt = DB.String txt `elem` map (valueAt label) docs


getGroupedAuctions :: [Auction] -> [[Auction]]
getGroupedAuctions = group . quicksort 

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
quicksort (p : xs) = quicksort lesser ++ [p] ++ quicksort greater
  where
    lesser  = filter (< p) xs
    greater = filter (>= p) xs

getAmount :: N.NBT -> Maybe N.NbtContents
getAmount = getNBTAttr "Count"

-- There are a couple of weird things. Some items can have a reforge that has the same name as the prefix of the item. I.e. superior and superior dragon boots. This will then be transformed into very
-- But in the NBT info it will still say the superior as the reforge and not very. Same goes for strong and wise. This causes names to be shortened in the wrong way
getReforge :: N.NBT -> Text
getReforge nbt = mod'
  where
    modifier    = getReforgeNBT nbt
    reforgeName = maybe "None" getStringTag modifier
    mod'        = if reforgeName `elem` weirdMods
        then fst $ T.span (`notElem` "_") reforgeName
        else reforgeName

getProperItemName :: Text -> Text -> Text
getProperItemName name reforge =
    if T.toLower reforge == "none" then name else getItemNameWithoutMod name

getItemNameWithoutMod :: Text -> Text
getItemNameWithoutMod txt = itemName
    where itemName = (dropN 1 . snd) $ T.span (`notElem` " ") txt

weirdMods :: [Text]
weirdMods = ["rich_sword", "odd_sword", "rich_bow", "odd_bow"]

getReforgeNBT :: N.NBT -> Maybe N.NbtContents
getReforgeNBT = getNBTAttr "modifier"

getStringTag :: N.NbtContents -> Text
getStringTag (N.StringTag txt) = txt

getInt :: N.NbtContents -> Int
getInt (N.ByteTag  n) = fromIntegral n
getInt (N.ShortTag n) = fromIntegral n
getInt (N.IntTag   n) = fromIntegral n
getInt (N.LongTag  n) = fromIntegral n
getInt _              = 0

getEnchants :: N.NbtContents -> [Text]
getEnchants (N.CompoundTag nbts) = [ nbtEnchantToText nbt | nbt <- nbts ]
  where
    nbtEnchantToText :: N.NBT -> Text
    nbtEnchantToText (N.NBT enchantName (N.IntTag n)) =
        T.pack $ T.unpack enchantName ++ " " ++ (intToRoman . fromIntegral) n

getEnchantsFromNbt :: N.NBT -> Maybe N.NbtContents
getEnchantsFromNbt = getNBTAttr "enchantments"

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
