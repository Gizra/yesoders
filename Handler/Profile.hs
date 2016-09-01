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


    -- @todo: Prevent own selection.
    let flaggedId = userId

    mFlagging <- runDB . getBy $ UniqueFlagMentor userId flaggedId

    let (action, flagMessage) =
            case mFlagging of
                Just _ -> (Unflag, "Unark as mentor" :: Text)
                Nothing -> (Flag, "Mark as mentor" :: Text)

    let token = getValidToken csrf flaggedId action

    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "user")
