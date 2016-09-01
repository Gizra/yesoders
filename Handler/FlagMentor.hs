module Handler.FlagMentor where

import Import
import Data.Digest.Pure.MD5
import qualified Data.Text as T (append)


getFlagMentorR :: UserId -> FlagAction-> Text -> Handler Html
getFlagMentorR flaggedId action token = do
    flaggedEntity <- runDB $ get404 flaggedId

    csrf <- fmap reqToken getRequest
    let csrf' = fromMaybe "" csrf
    -- Calculate the token of the Entity ID along with the action.
    -- We have to convert Text to ByteString, to md5, back to ByteString and
    -- finish with converting back to Text.
    let token' = getValidToken csrf flaggedId action

    if not $ token' == token
        then
            -- defaultLayout $ toWidget [hamlet|token should be #{token'}|]
            notFound
        else do
            let nextAction = if action == Flag then Unflag else Flag
            let nextToken = getValidToken csrf flaggedId nextAction
            defaultLayout $ do
                toWidget [hamlet|token is correct! Next action is #{show nextAction}, with token #{nextToken}|]

-- @todo: How to generalize UserId, to accept any Entity ID?
getValidToken :: Maybe Text -> UserId -> FlagAction -> Text
getValidToken csrf flaggedId action =
    -- Calculate the token of the Entity ID along with the action.
    -- We have to convert Text to ByteString, to md5, back to ByteString and
    -- finish with converting back to Text.
    pack $ show $ md5 . fromStrict $ encodeUtf8 $ csrf' `T.append` (pack $ show action) :: Text
    where csrf' = fromMaybe "" csrf
