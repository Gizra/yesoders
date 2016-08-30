module Handler.Home
    ( getHomeR
    ) where

import Import

getHomeR :: Handler Html
getHomeR = do
    -- @todo: how to add random - Must we use esqueleto with rand_?
    users <- runDB $ selectList [UserBlocked ==. False] [LimitTo 24] :: Handler [Entity User]
    defaultLayout $ do
        setTitle "Haskellers"
        $(widgetFile "homepage")
