module Handler.Root
    ( getRootR
    ) where

import Import

import Data.Time
import Data.Text.ICU.Normalize
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import Yesod.Form.Jquery

getRootR :: Handler Html
getRootR = do
    master <- getYesod
    (allProfs, len) <- liftIO . readIORef $ appHomepageProfiles master
    gen <- liftIO newStdGen
    news <- runDB $ selectList [] [Desc NewsWhen, LimitTo 1]
    now <- liftIO getCurrentTime
    let minus7d = addUTCTime ((-1) * 60 * 60 * 24 * 7) now
    job <- runDB $ selectList
        [JobPostedAt >. minus7d, JobOpen ==. True]
        [Desc JobPostedAt, LimitTo 1]
    let profs =
            if null allProfs
                then []
                else take 24 $ shuffle' allProfs len gen

    let fuzzyDiffTime = humanReadableTimeDiff now
    (public, private) <- runDB $ do
        public <- count [ UserVerifiedEmail ==. True
                        , UserVisible ==. True
                        , UserBlocked ==. False
                        ]
        private <- count [ UserVerifiedEmail ==. True
                         , UserVisible ==. False
                         , UserBlocked ==. False
                         ]
        return (public, private)
    defaultLayout $ do
        setTitle "Haskellers"
        addScriptEither $ urlJqueryJs master
        addScriptEither $ urlJqueryUiJs master
        addStylesheetEither $ urlJqueryUiCss master
        addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
        addScriptRemote "http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/src/markerclusterer.js"
        -- toWidget $(cassiusFile "templates/jobs.cassius")
        -- toWidget $(cassiusFile "templates/users.cassius")
        $(widgetFile "homepage")
