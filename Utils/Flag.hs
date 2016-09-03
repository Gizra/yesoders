module Utils.Flag
    ( getFlagToken
    , getFlagTokenFromCsrf
    , getFlagWidget
    ) where

import Import

import Data.Digest.Pure.MD5
import qualified Data.Text as T (append)
import Database.Persist.Sql (fromSqlKey)

-- | Get teh flag widget, if the current user has access to it.
getFlagWidget :: ( ToBackendKey SqlBackend val
                 , PersistEntity val
                 , val ~ User
                 , PersistEntityBackend a ~ SqlBackend
                 , PersistEntity a
                 , FlagMessage (Unique a)
                 )
              => Maybe (Key val) -- If the user is anonymous, we return Nothing
              -> Key val
              -> (Key val -> Key val -> Unique a)
              -> Handler (Maybe Widget)
getFlagWidget muid entityKey unique = do
    case muid of
        Just uid -> do
            let uniqueEntity = unique uid entityKey
            mFlagging <- runDB $ getBy uniqueEntity

            let action =
                    case mFlagging of
                        Just _ -> Unflag
                        Nothing -> Flag

            if (flagAccess uniqueEntity action)
                then do
                    let message = flagMessage uniqueEntity action
                    token <- getFlagTokenFromCsrf entityKey action
                    return $ Just $ do
                        toWidget [whamlet|
                            <div class="flag-wrapper action-#{toLower $ show action}">
                                <a href="@{FlagMentorR entityKey action token}">#{message}
                        |]
                        toWidget [julius|
                            $(function() {
                                $('.flag-wrapper a').click(function(e) {
                                    e.preventDefault();
                                    var $self = $(this);
                                    var url = $self.attr('href');

                                    // Call via Ajax.
                                    $.ajax ({
                                        type: "GET",
                                        url: url,
                                        success: function (res) {
                                            if (!res.length) {
                                                // Request was succesful, however the user doesn't
                                                // have access to the opposite action, so remove the flag.
                                                $self.parent().remove();
                                            }
                                            $self.attr('href', res.url);
                                            $self.html(res.message);
                                            // Change the action-[flag|unflag] class from the wrapper.
                                            $self.parent()
                                                .removeClass('action-flag')
                                                .removeClass('action-unflag')
                                                .addClass('action-' + res.action)
                                        },
                                        error: function (res) { console.log(res); }
                                    });
                                });
                            });

                        |]
                else
                    return Nothing
        Nothing ->
            return Nothing


-- | Get flag token from the user's CSRF.
getFlagTokenFromCsrf :: ToBackendKey SqlBackend record => Key record -> FlagAction -> Handler Text
getFlagTokenFromCsrf entityKey action = do
    csrf <- fmap reqToken getRequest
    return $ getFlagToken csrf entityKey action


-- | Get flag token, with a provided CSRF token.
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
