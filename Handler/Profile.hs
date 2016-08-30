module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    (userId, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "user")

postProfileR :: Handler Html
postProfileR = error "Not yet implemented: postProfileR"
