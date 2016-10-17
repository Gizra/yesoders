module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Model.Types           as X
import Settings              as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Auth            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Form.Fields     as X (Textarea (..))
import Yesod.Test            as X

import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.List as DL
import qualified Data.Text.Lazy as TL
import qualified Test.HUnit as HUnit
import Network.Wai.Test hiding (assertHeader, assertNoHeader, request)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)


withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables


bodyNotContains :: String -> YesodExample site ()
bodyNotContains text = withResponse $ \ res ->
  liftIO $ HUnit.assertBool ("Expected body not to contain " ++ text) $
    not $ contains (simpleBody res) text

contains :: BSL8.ByteString -> String -> Bool
contains a b = DL.isInfixOf b (TL.unpack $ decodeUtf8 a)

authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create an active user.
createUser :: Text -> YesodExample App (Entity User)
createUser ident = do
    insertUser ident False False

-- | Create a blocked user.
createBlockedUser :: Text -> YesodExample App (Entity User)
createBlockedUser ident =
    insertUser ident True False

-- | Create a blocked user.
createAdminUser :: Text -> YesodExample App (Entity User)
createAdminUser ident =
    insertUser ident True True


-- | Create a user.
insertUser :: Text -> Bool -> Bool -> YesodExample App (Entity User)
insertUser ident isBlocked isAdmin = do
    currentTime <- liftIO getCurrentTime
    runDB $ insertEntity User
        { userIdent = ident
        , userEmail = ident ++ ("@example.com" :: Text)
        , userFullName = Nothing
        , userDesc = Just $ Textarea ("user description of " ++ ident)
        , userAdmin = isAdmin
        , userEmployment = NotLooking
        , userBlocked = isBlocked
        , userEmailPublic = False
        , userCreated = currentTime
        }
