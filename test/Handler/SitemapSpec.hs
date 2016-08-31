module Handler.SitemapSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "sitemap.xml" $ do
        it "loads the sitemap and shows users" $ do
            user <- createUser "foo"
            get SitemapR
            statusIs 200

            let (Entity _ u) = user
            bodyContains . unpack $ userIdent u
