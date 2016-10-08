module Handler.EditUserSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do

    describe "getEditUserR" $ do
        it "asserts no access to user edit for anonymous users" $ do
            _ <- createUser "foo"
            get $ EditUserR "foo"
            statusIs 403

    --
    -- describe "postEditUserR" $ do
    --     error "Spec not implemented: postEditUserR"
