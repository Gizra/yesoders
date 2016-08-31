module Handler.Sitemap where

import Import
import Yesod.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR = do
    users <- runDB $ selectList [] [Asc UserId]
    sitemapList $ map go users
    where
        go (Entity _ user) =
            SitemapUrl (UserR $ userIdent user) (Just (userCreated user)) (Just Daily) (Just 0.9)
