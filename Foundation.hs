{-# LANGUAGE DeriveGeneric #-}
module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Text.Read            (readMaybe)
import Yesod.Auth.Dummy
import Yesod.Auth.OAuth2.Github
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import Yesod.Form.Jquery

import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Data.Char as C (isSpace, toUpper, toLower)
import Data.Time

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

data FlagAction = Unflag | Flag
    deriving (Enum, Eq, Generic, Read, Show)

instance ToJSON FlagAction where

instance PathPiece FlagAction where
    fromPathPiece = readMaybe . capitalized .unpack
        where capitalized :: String -> String
              capitalized (x : xs) = C.toUpper x : fmap C.toLower xs
              capitalized [] = []

    toPathPiece = pack . (fmap C.toLower) . show

class FlagMessage a where
    flagMessage :: a -> FlagAction -> Text
    flagAccess :: a -> FlagAction -> Bool


instance FlagMessage (Unique FlagMentor) where
    flagMessage _ Unflag = "Unflag mentor"
    flagMessage _ Flag = "Flag mentor"

    flagAccess _ Unflag = True
    -- Make sure user can't mark themself as own mentors.
    flagAccess (UniqueFlagMentor uid flaggedId) Flag = uid /= flaggedId


-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        (title, parents) <- breadcrumbs
        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                  , NavbarLeft $ MenuItem
                    { menuItemLabel = "Your Profile"
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback = isJust muser
                    }
                  , NavbarRight $ MenuItem
                    { menuItemLabel = "GitHub Login"
                    , menuItemRoute = AuthR $ PluginR "github" ["forward"]
                    , menuItemAccessCallback = isNothing muser
                    }
                  , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                  ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        pc <- widgetToPageContent $ do
            -- Semantic UI
            addStylesheet $ StaticR semantic_semantic_min_css
            addScript $ StaticR semantic_sidebar_min_js
            addScript $ StaticR semantic_transition_min_js
            addScript $ StaticR semantic_visibility_min_js

            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized SitemapR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (UserR _) _ = return Authorized

    isAuthorized (AuthR LogoutR) _ = isAuthenticated
    isAuthorized (AuthR _) _ = do
        mu <- maybeAuthId
        return $ case mu of
            Nothing -> Authorized
            Just _ -> Unauthorized "As a logged in user, you cannot re-login. You must Logout first."
    isAuthorized (FlagMentorR _ _ _) _ = isAuthenticated
    isAuthorized ProfileR _ = isAuthenticated

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

isAuthenticated :: Handler AuthResult
isAuthenticated = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = do
        currentTime <- liftIO getCurrentTime
        runDB $ case credsPlugin creds of
            "github" -> do
                let ident = fromMaybe "" $ lookup "login" $ credsExtra creds
                x <- getBy . UniqueUser $ ident
                case x of
                    Just (Entity uid _) -> return $ Authenticated uid
                    Nothing -> Authenticated <$> createUser ident (fromMaybe "" $ lookup "email" $ credsExtra creds) Nothing currentTime
            "dummy" -> do
                let ident = credsIdent creds
                x <- getBy . UniqueUser $ ident
                case x of
                    Just (Entity uid _) -> return $ Authenticated uid
                    Nothing -> Authenticated <$> createUser ident (ident ++ "@example.com") (Just ident) currentTime

            _ -> error "authenticate function does not know this authentication provider. Did you define it?"

        where createUser ident email fullName currentTime =
                insert User
                    { userIdent = ident
                    -- Extract the email from the GitHub's response
                    , userEmail = email
                    -- @todo: Get from GitHub
                    , userFullName = fullName
                    , userDesc = Nothing
                    , userAdmin = False
                    , userEmployment = Nothing
                    , userBlocked = False
                    , userEmailPublic = False
                    , userCreated = currentTime
                    }



    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins app =
        [ oauth2Github (oauthKeysClientId githubKeys) (oauthKeysClientSecret githubKeys)
        ] ++ extraAuthPlugins
        where githubKeys = appGithubKeys $ appSettings app
              -- Enable authDummy login when in development mode.
              extraAuthPlugins = [authDummy | appDevelopment $ appSettings app]

    authHttpManager = getHttpManager

instance YesodBreadcrumbs App where
  breadcrumb HomeR      = return ("Home", Nothing)
  breadcrumb ProfileR = return ("Your Profile", Just HomeR)
  breadcrumb  _ = return ("home", Nothing)

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

instance YesodJquery App where
    urlJqueryUiCss _ = Left $ StaticR jquery_ui_css

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

-- Helper functions

prettyDay :: Day -> String
prettyDay = formatTime defaultTimeLocale "%B %e, %Y"

humanReadableTimeDiff :: UTCTime     -- ^ current time
                      -> UTCTime     -- ^ old time
                      -> String
humanReadableTimeDiff curTime oldTime =
    helper diff
  where
    diff    = diffUTCTime curTime oldTime

    minutes :: NominalDiffTime -> Double
    minutes n = realToFrac $ n / 60

    hours :: NominalDiffTime -> Double
    hours   n = (minutes n) / 60

    days :: NominalDiffTime -> Double
    days    n = (hours n) / 24

    weeks :: NominalDiffTime -> Double
    weeks   n = (days n) / 7

    years :: NominalDiffTime -> Double
    years   n = (days n) / 365

    i2s :: RealFrac a => a -> String
    i2s n = show m where m = truncate n :: Int

    old = utcToLocalTime utc oldTime

    trim = f . f where f = reverse . dropWhile C.isSpace

    dow           = trim $! formatTime defaultTimeLocale "%l:%M %p on %A" old
    thisYear      = trim $! formatTime defaultTimeLocale "%b %e" old
    previousYears = trim $! formatTime defaultTimeLocale "%b %e, %Y" old

    helper  d | d < 1          = "one second ago"
              | d < 60         = i2s d ++ " seconds ago"
              | minutes d < 2  = "one minute ago"
              | minutes d < 60 = i2s (minutes d) ++ " minutes ago"
              | hours d < 2    = "one hour ago"
              | hours d < 24   = i2s (hours d) ++ " hours ago"
              | days d < 5     = dow
              | days d < 10    = i2s (days d)  ++ " days ago"
              | weeks d < 2    = i2s (weeks d) ++ " week ago"
              | weeks d < 5    = i2s (weeks d)  ++ " weeks ago"
              | years d < 1    = thisYear
              | otherwise      = previousYears
