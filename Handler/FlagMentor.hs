module Handler.FlagMentor where

import Import

import qualified Data.Char as C (toLower)
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


                    -- Prepare the response that will hold the opposite action,
                    -- if accessible.
                    let nextAction = if action == Flag then Unflag else Flag

                    if flagAccess uniqueEntity nextAction
                        then do
                            -- User has access for the opposite action.
                            nextToken <- getFlagTokenFromCsrf flaggedId nextAction
                            urlRender <- getUrlRender

                            -- Return JSON with the link to the next action.
                            return $ object
                                [ "action" .= String (pack $ fmap C.toLower $ show nextAction)
                                , "url" .= String (urlRender $ FlagMentorR flaggedId nextAction nextToken)
                                , "message" .= String (flagMessage uniqueEntity nextAction)
                                ]
                        else
                            -- Return an empty object, but with HTTP response code
                            -- 200.
                            return $ object []
        else
            -- User reached this route, but has no access to the action.
            notFound
