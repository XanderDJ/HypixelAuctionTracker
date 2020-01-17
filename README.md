# HypixelAuctionTracker

This project will get the active items on the hypixel skyblock auctions and store them into a mongoDB. Using this data many interesting statistics can be visualized and used in a website for skyblock players.

The most simple statistic and the reason I started this project was figuring out the average value of a certain item on the auction. This way you can make sure you're not overpaying if you're not in direct need of something.

# DONE
    Running this program will get all items on auction. Group them and add important information from the nbt to each auction element. Then it will store each group of auction items in their own collection. This program conforms to the api limit from hypixel when the total pages on the auction are at most 120 (max api calls per sec = 10. the main thread runs every minute.)
    If skyblock suddenly gets massively more popular (120k items on ah) then this program will need to reduce it's threads from 10 to 2.

# Todo
    Handle db exceptions. Only one type of exception is handled (poorly) and that is when items get inserted to the db. If the db is offline for a long period then all threads should wait untill connections are restored. Now it will just discard all items when the db is unavailable. In essence this results in the same thing (items not being updated) but the better way will result in less processing power being used and less data being used (no api calls when db isn't online).