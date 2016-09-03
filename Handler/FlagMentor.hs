module Handler.FlagMentor where

import Import

import Utils.Flag (getFlagTokenFromCsrf)


getFlagMentorR :: UserId -> FlagAction-> Text -> Handler Value
getFlagMentorR flaggedId action token = do
    _ <- runDB $ get404 flaggedId
    authId <- requireAuthId

    token' <- getFlagTokenFromCsrf flaggedId action

    let uniqueEntity = UniqueFlagMentor authId flaggedId
    if flagAccess uniqueEntity action
        then
            if token' /= token
                then
                    -- defaultLayout $ toWidget [hamlet|token should be #{token'}|]
                    notFound
                else do
                    -- Insert a new flagging or delete an existing one, baesd on the
                    -- action.
                    _ <- case action of
                            Unflag -> do
                                runDB $ deleteBy uniqueEntity

                            Flag -> do
                                currentTime <- liftIO getCurrentTime
                                _ <- runDB $ insertUnique FlagMentor
                                    { flagMentorUser = authId
                                    , flagMentorFlagging = flaggedId
                                    , flagMentorCreated = currentTime
                                    }
                                return ()

                    let nextAction = if action == Flag then Unflag else Flag
                    nextToken <- getFlagTokenFromCsrf flaggedId nextAction

                    urlRender <- getUrlRender

                    -- Return JSON with the link to the next action.
                    return $ object
                        [ "action" .= nextAction
                        , "url" .= String (urlRender $ FlagMentorR flaggedId nextAction nextToken)
                        ]
        else
            -- @todo: Change to access denied.
            notFound
