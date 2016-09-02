module Handler.Profile where

import Import
import Utils.Flag (getFlagWidget)

getProfileR :: Handler Html
getProfileR = do
    currentTime <- liftIO getCurrentTime
    (userId, user) <- requireAuthPair
    let memberFor = humanReadableTimeDiff currentTime (userCreated user)

    -- @todo: Prevent own selection.
    let flaggedId = userId

    flagWidget <- getFlagWidget userId flaggedId UniqueFlagMentor

    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "user")
