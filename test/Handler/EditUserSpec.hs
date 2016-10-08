module Handler.EditUserSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getEditUserR" $ do
        it "asserts no access to user edit for anonymous users" $ do
            _ <- createUser "foo"
            get $ EditUserR "foo"
            statusIs 403

        it "asserts non access to user edit for non owner" $ do
            _ <- createUser "foo"
            nonOwner <- createUser "bar"
            authenticateAs nonOwner
            get $ EditUserR "foo"
            statusIs 403

        it "asserts access to user edit for owner" $ do
            user <- createUser "foo"
            authenticateAs user
            get $ EditUserR "foo"
            statusIs 200

        it "asserts access to user edit for admin user" $ do
            _ <- createUser "foo"
            adminUser <- createAdminUser "bar"
            authenticateAs adminUser
            get $ EditUserR "foo"
            statusIs 200
