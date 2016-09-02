module Utils.Flag
    ( getFlagToken
    , getFlagTokenFromCsrf
    , getFlagWidget
    ) where

import Import

import Data.Digest.Pure.MD5
import qualified Data.Text as T (append)
import Database.Persist.Sql (fromSqlKey)

-- @todo: Get the flagging messages as argument
-- (Key record -> Key record -> Unique b)
getFlagWidget :: ( ToBackendKey SqlBackend val, PersistEntity val, val ~ User)
              => Key val
              -> Entity val
              -> Handler Widget
getFlagWidget muid entity = do
    let (Entity entityKey entityVal) = entity
    mFlagging <- runDB . getBy $ UniqueFlagMentor muid entityKey

    let (action, flagMessage) =
            case mFlagging of
                Just _ -> (Unflag, "Unark as mentor" :: Text)
                Nothing -> (Flag, "Mark as mentor" :: Text)

    token <- getFlagTokenFromCsrf entityKey action

    return [whamlet|<a href="@{FlagMentorR entityKey action token}">#{flagMessage}|]


-- | Get flag token from the user's CSRF.
getFlagTokenFromCsrf :: ToBackendKey SqlBackend record => Key record -> FlagAction -> Handler Text
getFlagTokenFromCsrf entityKey action = do
    csrf <- fmap reqToken getRequest
    return $ getFlagToken csrf entityKey action


getFlagToken :: ToBackendKey SqlBackend record => Maybe Text -> Key record -> FlagAction -> Text
getFlagToken csrf entityKey action =
    -- Calculate the token of the Entity ID along with the action.
    -- We have to convert Text to ByteString, to md5, back to ByteString and
    -- finish with converting back to Text.
    pack $ show $ md5 . fromStrict $ encodeUtf8 $ csrf' `T.append` actionText `T.append` keyText
    where
        csrf' = fromMaybe "" csrf
        actionText = pack $ show action :: Text
        keyText = pack . show $ fromSqlKey entityKey :: Text
