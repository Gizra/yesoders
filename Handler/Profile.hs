module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    currentTime <- liftIO getCurrentTime
    (userId, user) <- requireAuthPair
    let memberFor = humanReadableTimeDiff currentTime (userCreated user)

    -- @todo: Use helper function.
    csrf <- fmap reqToken getRequest
    let csrf' = fromMaybe "" csrf
    let token = getValidToken csrf userId Flag

    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "user")
