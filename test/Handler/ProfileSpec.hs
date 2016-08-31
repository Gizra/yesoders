module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "asserts no access to my-account for anonymous users" $ do
        get ProfileR
        statusIs 403

    it "asserts access to my-account for authenticated users" $ do
        user <- createUser "foo"
        authenticateAs user

        get ProfileR
        statusIs 200

    it "asserts shows user's information" $ do
        user <- createUser "bar"
        authenticateAs user

        get ProfileR
        let (Entity _ u) = user
        -- Member for
        htmlAnyContain ".ui.segment div" "Members for"

        -- User's GitHub name
        htmlAnyContain "a.header" . unpack $ userIdent u
