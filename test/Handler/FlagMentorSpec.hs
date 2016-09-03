module Handler.FlagMentorSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getFlagMentorR" $ do
        it "asserts flag link doesn't show to own user" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            get ProfileR
            htmlCount ".flag-wrapper" 0

        it "asserts flag link doesn't show to anonymous users" $ do
            userEntity <- createUser "foo"
            let (Entity _ user) = userEntity

            get $ UserR (userIdent user)
            htmlCount ".flag-wrapper" 1
