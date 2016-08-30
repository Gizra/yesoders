module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "asserts access to my-account for authenticated user" $ do
        user <- createUser "foo"
        authenticateAs user

        get ProfileR
        statusIs 200
