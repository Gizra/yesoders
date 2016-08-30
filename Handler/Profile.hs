module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    currentTime <- liftIO getCurrentTime
    (_, user) <- requireAuthPair
    let memberFor = humanReadableTimeDiff currentTime (userCreated user)
    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "user")

postProfileR :: Handler Html
postProfileR = error "Not yet implemented: postProfileR"
