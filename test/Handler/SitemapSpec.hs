module Handler.SitemapSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "sitemap.xml" $ do
        it "loads the sitemap and shows active users" $ do
            user <- createUser "foo"
            blockedUser <- createBlockedUser "bar"
            get SitemapR
            statusIs 200

            let (Entity _ u) = user
            let (Entity _ bu) = blockedUser

            bodyContains . unpack $ userIdent u
            bodyNotContains . unpack $ userIdent bu
