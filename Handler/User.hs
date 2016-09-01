module Handler.User where

import Import

getUserR :: Text -> Handler Html
getUserR ident = do
    userEntity <- runDB . getBy404 $ UniqueUser ident

    let (Entity userId user) = userEntity
    currentTime <- liftIO getCurrentTime

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
