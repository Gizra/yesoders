module Handler.FlagMentor where

import Import
import Data.Digest.Pure.MD5
import qualified Data.Text as T (append)


getFlagMentorR :: UserId -> FlagAction-> Text -> Handler Value
getFlagMentorR flaggedId action token = do
    flaggedEntity <- runDB $ get404 flaggedId
    authId <- requireAuthId

    csrf <- fmap reqToken getRequest
    let csrf' = fromMaybe "" csrf
    -- Calculate the token of the Entity ID along with the action.
    -- We have to convert Text to ByteString, to md5, back to ByteString and
    -- finish with converting back to Text.
    let token' = getValidToken csrf flaggedId action

    if token' /= token
        then
            -- defaultLayout $ toWidget [hamlet|token should be #{token'}|]
            notFound
        else do
            -- Insert a new flagging or delete an existing one, baesd on the
            -- action.
            _ <- case action of
                    Unflag -> do
                        -- mFlagging <- getBy $ FlagMentorUnique authId flaggedId
                        return ()
                        -- case mFlagging of
                        --     Just flagging -> do
                        --         let (Entity _ key) = flagging
                        --         runDB $ delete key
                        --     Nothing -> return ()

                    Flag -> do
                        currentTime <- liftIO getCurrentTime
                        _ <- runDB $ insert FlagMentor
                            { flagMentorUser = authId
                            , flagMentorFlagging = flaggedId
                            , flagMentorCreated = currentTime

                            }
                        return ()

            let nextAction = if action == Flag then Unflag else Flag
            let nextToken = getValidToken csrf flaggedId nextAction

            urlRender <- getUrlRender

            -- Return JSON with the link to the next action.
            return $ object
                [ "action" .= nextAction
                -- , "data" .= urlRender $ FlagMentorR flaggedId nextAction nextToken
                ]

getValidToken :: Maybe Text -> Key record -> FlagAction -> Text
getValidToken csrf flaggedId action =
    -- Calculate the token of the Entity ID along with the action.
    -- We have to convert Text to ByteString, to md5, back to ByteString and
    -- finish with converting back to Text.
    pack $ show $ md5 . fromStrict $ encodeUtf8 $ csrf' `T.append` (pack $ show action) :: Text
    where csrf' = fromMaybe "" csrf
