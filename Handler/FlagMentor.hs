module Handler.FlagMentor where

import Import

import Utils.Flag (getFlagTokenFromCsrf)


getFlagMentorR :: UserId -> FlagAction-> Text -> Handler Value
getFlagMentorR flaggedId action token = do
    _ <- runDB $ get404 flaggedId
    authId <- requireAuthId

    token' <- getFlagTokenFromCsrf flaggedId action

    if token' /= token
        then
            -- defaultLayout $ toWidget [hamlet|token should be #{token'}|]
            notFound
        else do
            -- Insert a new flagging or delete an existing one, baesd on the
            -- action.
            _ <- case action of
                    Unflag -> do
                        runDB . deleteBy $ UniqueFlagMentor authId flaggedId

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
                -- , "data" .= urlRender $ FlagMentorR flaggedId nextAction nextToken
                ]
