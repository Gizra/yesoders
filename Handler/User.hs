module Handler.User where

import           Import

import           Database.Esqueleto ((&&.), (?.), (^.))
import qualified Database.Esqueleto as E
import           Utils.Flag         (getFlagWidget)
import           Utils.LocalTasks

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

    mlocalTasks <- getLocalTasksWidget
            [ ("View", UserR ident)
            , ("Edit", UserUpdateR ident)
            ]

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
