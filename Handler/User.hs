module Handler.User where

import Import

import Utils.Flag (getFlagWidget)

getProfileR :: Handler Html
getProfileR = do
    (userId, user) <- requireAuthPair
    getUserR (userIdent user)

getUserR :: Text -> Handler Html
getUserR ident = do
    (Entity userId user) <- runDB . getBy404 $ UniqueUser ident
    currentTime <- liftIO getCurrentTime
    let memberFor = humanReadableTimeDiff currentTime (userCreated user)

    muid <- maybeAuthId

    mflagWidget <- getFlagWidget muid userId UniqueFlagMentor

    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "user")
