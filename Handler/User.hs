module Handler.User where

import Import

import qualified Database.Esqueleto as E
import           Database.Esqueleto       ((^.), (?.), (&&.))
import           Utils.Flag               (getFlagWidget)

getProfileR :: Handler Html
getProfileR = do
    (userId, user) <- requireAuthPair
    getUserR (userIdent user)

getUserR :: Text -> Handler Html
getUserR ident = do
    (Entity userId user) <- runDB . getBy404 $ UniqueUser ident
    currentTime <- liftIO getCurrentTime
    let memberFor = humanReadableTimeDiff currentTime (userCreated user)

    muid <- maybeAuthId

    mflagWidget <- getFlagWidget muid userId UniqueFlagMentor

    mentors <- getUserMentors userId

    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"

        $(widgetFile "user")

getUserMentors :: Key User -> Handler [(E.Value (Key User), E.Value Text)]
getUserMentors uid =
    runDB $ E.select
           . E.from $ \(flagMentor `E.InnerJoin` user) -> do
                E.on $ flagMentor ^. FlagMentorFlagging E.==. user ^. UserId
                E.where_ $ flagMentor ^. FlagMentorUser E.==. E.val uid
                E.limit 25
                return
                    ( user ^. UserId
                    , user ^. UserIdent
                    )


userForm :: User -> Form User
userForm ident user = renderSematnicUiDivs $ User
    <$> pure ident
    <*> pure (userEmail user)
    <*> aopt textField "Full name" (userFullName <$> user)
    <*> aopt textField "Description" (userDesc <$> user)
    <*> pure (userAdmin user)
    <*> areq (selectField optionsEnum) (selectSettings "Item") (userEmployment <$> user)
    <*> aopt checkBoxField "Blocked"  (userBlocked <$> user)
    <*> aopt checkBoxField "Public email"  (userEmailPublic <$> user)
    <*> lift (liftIO getCurrentTime)
    where
        selectSettings label =
            FieldSettings
                { fsLabel = label
                , fsTooltip = Nothing
                , fsId = Nothing
                , fsName = Nothing
                , fsAttrs = [("class", "ui fluid dropdown")]
                }
