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
    let token' = pack $ show $ md5 . fromStrict $ encodeUtf8 $ csrf' `T.append` (pack $ show action) :: Text

    if not $ token' == token
        then
            -- defaultLayout $ toWidget [hamlet|token should be #{token'}|]
            notFound
        else
            defaultLayout $ do
                toWidget [hamlet|token is correct!|]
