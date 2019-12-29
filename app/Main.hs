{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import           Lib
import           Network.HTTP.Client     hiding ( host )
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status      ( statusCode )
import qualified Data.ByteString.Lazy          as B
import           Data.Aeson              hiding ( Value )
import           Data.List               hiding ( delete )
import           Data.Int
import           Data.Maybe
import qualified Data.Text                     as T
import           Text.Regex.TDFA
import           Database.MongoDB        hiding ( find
                                                , group
                                                , sort
                                                )
import qualified Database.MongoDB              as DB
                                                ( find
                                                , group
                                                , sort
                                                )
import           Control.Monad.Trans            ( liftIO )

main :: IO ()
main = do
    ap <- getAuctionPage 0
    print
        $ toBSON
        . head
        . convertToAuctionGroup
        . getGroupedAuctions
        $ updateAuctionPage ap
              

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
        uuid :: T.Text,   -- Part of index for the updates
        startAuction :: Int64, -- Part of index for the updates
        estimatedEnd :: Int64,
        itemName :: T.Text,
        itemLore :: T.Text,
        reforge :: T.Text,
        enchants :: [T.Text],
        hpb ::Int,
        extra :: T.Text,
        category :: T.Text,
        tier :: T.Text,
        startingBid :: Int,
        claimed :: Bool,
        highestBidAmount :: Int,
        bids :: [Bid]
    } deriving Show

data AuctionGroup = AuctionGroup {
    gItemName :: T.Text,
    gAuctions :: [Auction]
} deriving Show

data Bid =
    Bid {
        auctionId :: T.Text,
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
        startingBid      <- jsn .: "starting_bid"
        claimed          <- jsn .: "claimed"
        highestBidAmount <- jsn .: "highest_bid_amount"
        bids             <- jsn .: "bids"
        bidsList         <- mapM parseJSON bids
        return $ Auction uuid
                         (fromIntegral startAuction)
                         (fromIntegral estimatedEnd)
                         itemName
                         itemLore
                         "None"
                         ["None"]
                         0
                         extra
                         category
                         tier
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
        , "enchants" =: enchants ah
        , "startBid" =: startingBid ah
        , "highestBid" =: highestBidAmount ah
        , "bids" =: map toBSON (bids ah)
        ]

instance ToBSON Bid where
    toBSON bid = ["amount" =: amount bid, "timestamp" =: (MongoStamp . ts) bid]

-- Functions


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


quicksort :: Ord a => [a] -> [a]
quicksort []       = []
quicksort (p : xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser  = filter (< p) xs
    greater = filter (>= p) xs

getItemNames :: Maybe AuctionPage -> [T.Text]
getItemNames (Just ap) = [ itemName auction | auction <- (auctions ap) ]
getItemNames _         = ["Empty"]

getGroupedAuctions :: Maybe AuctionPage -> [[Auction]]
getGroupedAuctions (Just ap) = group . quicksort $ auctions ap
getGroupedAuctions _         = []

convertToAuctionGroup :: [[Auction]] -> [AuctionGroup]
convertToAuctionGroup groupedAuctions =
    [ AuctionGroup { gItemName = newItemName auctionGroup
                   , gAuctions = auctionGroup
                   }
    | auctionGroup <- groupedAuctions
    ]
    where newItemName = itemName . head

updateAuctionPage :: Maybe AuctionPage -> Maybe AuctionPage
updateAuctionPage (Just ap) = Just $ ap { auctions = newAuctions }
    where newAuctions = map (addReforge . addEnchants . addHpb) $ auctions ap

addReforge :: Auction -> Auction
addReforge ah = ah { reforge = reforgeName, itemName = newItemName }
  where
    oldItemName = itemName ah
    tempReforgeName =
        T.pack (((T.unpack oldItemName) :: String) =~ ("[A-Za-z]+" :: String))
    reforgeName =
        if elem tempReforgeName allReforges then tempReforgeName else reforge ah
    newItemName = if reforgeName /= reforge ah
        then dropN ((T.length reforgeName) + 1) oldItemName
        else oldItemName

addEnchants :: Auction -> Auction
addEnchants ah = ah { enchants = newEnchants }
  where
    extraInfo = T.unpack $ itemLore ah
    enchantsStr =
        getAllTextMatches ((extraInfo :: String) =~ ("9[A-Za-z ]+" :: String)) :: [ String
            ]
    newEnchants = if length enchantsStr /= 0
        then filter (`elem` allEnchants) $ map (dropN 1 . T.pack) enchantsStr
        else ["None"]

addHpb :: Auction -> Auction
addHpb ah = ah { hpb = newHpb } where newHpb = round $ getHpbAmount ah

getHpbAmount :: Auction -> Double
getHpbAmount ah | cat == "armor"  = amount / 4.0
                | cat == "weapon" = amount / 2.0
                | otherwise       = 0
  where
    cat    = category ah
    lore   = T.unpack $ itemLore ah
    amount = read $ matchHpb lore

getSubMatch :: (String, String, String, [String]) -> [String]
getSubMatch (_, _, _, el) = el

matchHpb :: String -> String
matchHpb lore = head subMatch
  where
    match =
        (lore :: String) =~ ("\\(\\+([0-9]+)\\)" :: String) :: ( String
            , String
            , String
            , [String]
            )
    stub     = getSubMatch match
    subMatch = if length stub /= 0 then stub else ["0"]

dropN :: Int -> T.Text -> T.Text
dropN n ""   = T.empty
dropN 0 text = text
dropN n text = dropN (n - 1) $ T.tail text


allReforges :: [T.Text]
allReforges =
    [ "Bizarre"
    , "Clean"
    , "Deadly"
    , "Demonic"
    , "Epic"
    , "Fair"
    , "Fast"
    , "Fierce"
    , "Fine"
    , "Forceful"
    , "Gentle"
    , "Godly"
    , "Grand"
    , "Hasty"
    , "Heavy"
    , "Heroic"
    , "Hurtful"
    , "Itchy"
    , "Keen"
    , "Legendary"
    , "Light"
    , "Mythic"
    , "Neat"
    , "Odd"
    , "Ominous"
    , "Pleasant"
    , "Pretty"
    , "Pure"
    , "Rapid"
    , "Rich"
    , "Shiny"
    , "Simple"
    , "Smart"
    , "Spicy"
    , "Strange"
    , "Strong"
    , "Superior"
    , "Titanic"
    , "Unpleasant"
    , "Unreal"
    , "Vivid"
    , "Wise"
    , "Zealous"
    ]

convMap = [(10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

intToRoman :: Int -> String
intToRoman = concat . unfoldr findLeast
  where
    findLeast n = case i of
        Just (x, r) -> Just (r, n - x)
        Nothing     -> Nothing
        where i = find (\(val, _) -> val <= n) convMap

enchantsMaxLevel :: [(String, Int)]
enchantsMaxLevel =
    [ ("Cleave"               , 5)
    , ("Critical"             , 6)
    , ("Cubism"               , 5)
    , ("Ender Slayer"         , 6)
    , ("Execute"              , 5)
    , ("Experience"           , 4)
    , ("First Strike"         , 4)
    , ("Giant Killer"         , 6)
    , ("Impaling"             , 3)
    , ("Lethality"            , 5)
    , ("Life Steal"           , 4)
    , ("Luck"                 , 6)
    , ("Scavenger"            , 4)
    , ("Thunderlord"          , 5)
    , ("Telekinesis"          , 1)
    , ("Vampirism"            , 6)
    , ("Venomous"             , 5)
    , ("Growth"               , 6)
    , ("Sugar Rush"           , 3)
    , ("True Protection"      , 1)
    , ("Aiming"               , 5)
    , ("Dragon Hunter"        , 5)
    , ("Impaling"             , 3)
    , ("Infinite Quiver"      , 5)
    , ("Piercing"             , 1)
    , ("Snipe"                , 3)
    , ("Experience"           , 3)
    , ("Harvesting"           , 5)
    , ("Rainbow"              , 1)
    , ("Smelting Touch"       , 1)
    , ("Angler"               , 6)
    , ("Blessing"             , 5)
    , ("Caster"               , 6)
    , ("Frail"                , 6)
    , ("Magnet"               , 6)
    , ("Spiked Hook"          , 6)
    , ("Bane of Anthropods"   , 6)
    , ("Fire Aspect"          , 2)
    , ("Looting"              , 4)
    , ("Knockback"            , 2)
    , ("Sharpness"            , 6)
    , ("Smite"                , 6)
    , ("Aqua Affinity"        , 1)
    , ("Blast Protection"     , 5)
    , ("Depth Strider"        , 3)
    , ("Feather Falling"      , 5)
    , ("Fire Protection"      , 5)
    , ("Frost Walker"         , 2)
    , ("Projectile Protection", 5)
    , ("Protection"           , 6)
    , ("Respiration"          , 3)
    , ("Thorns"               , 3)
    , ("Flame"                , 1)
    , ("Power"                , 6)
    , ("Punch"                , 2)
    , ("Efficiency"           , 5)
    , ("Fortune"              , 3)
    , ("Silk Touch"           , 1)
    , ("Lure"                 , 6)
    , ("Luck of the Sea"      , 6)
    ]

enchantVariants :: (String, Int) -> [T.Text]
enchantVariants (str, maxLevel) =
    [ T.pack $ str ++ " " ++ intToRoman lvl | lvl <- [1 .. maxLevel] ]

allEnchants :: [T.Text]
allEnchants = concatMap enchantVariants enchantsMaxLevel


