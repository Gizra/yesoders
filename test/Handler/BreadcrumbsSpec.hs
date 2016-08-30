module Handler.BreadcrumbsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "does not have the breadcrumbs on the homepage" $ do
        get HomeR
        statusIs 200
        htmlCount ".breadcrumb" 0
