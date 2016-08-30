module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "asserts no access to my-account for anonymous user" $ do
        get ProfileR
        statusIs 403
