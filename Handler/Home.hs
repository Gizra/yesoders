module Handler.Home
    ( getHomeR
    ) where

import Import

import Data.Time
import Data.Text.ICU.Normalize
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import Yesod.Form.Jquery

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Char as C (toLower, isSpace, isMark)

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuthPair
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
        $(widgetFile "homepage")


-- Get Route out of a Profile
profileUserR :: Profile -> Route App
profileUserR p = userR ((profileUserId p, profileUser p), profileUsername p)

-- Get gravatar for a user.
gravatar :: Int -> Text -> Text
gravatar s x = T.concat
    [ "http://www.gravatar.com/avatar/"
    , hash
    , "?d=identicon&s="
    , pack $ show s
    ]
  where
    hash = pack $ show $ md5 $ L.fromString $ map C.toLower $ trim $ unpack x
    trim = reverse . dropWhile C.isSpace . reverse . dropWhile C.isSpace
