module Handler.User where

import Import

import Utils.Flag (getFlagTokenFromCsrf)

getUserR :: Text -> Handler Html
getUserR = error "Implement"
-- getUserR ident = do
--     userEntity <- runDB . getBy404 $ UniqueUser ident
--     let (Entity userId user) = userEntity
--
--     mCurrentUid <- maybeAuthId
--
--     currentTime <- liftIO getCurrentTime
--
--     let memberFor = humanReadableTimeDiff currentTime (userCreated user)
--
--
--     -- @todo: Prevent own selection.
--     let flaggedId = userId
--
--     mFlagging <- runDB . getBy $ UniqueFlagMentor userId flaggedId
--
--     let (action, flagMessage) =
--             case mFlagging of
--                 Just _ -> (Unflag, "Unark as mentor" :: Text)
--                 Nothing -> (Flag, "Mark as mentor" :: Text)
--
--     token <- getFlagTokenFromCsrf
--
--     defaultLayout $ do
--         setTitle . toHtml $ userIdent user `mappend` "'s User page"
--         $(widgetFile "user")
