module Utils.LocalTasks
    (getLocalTasksWidget) where

import Import

-- | Get the flag widget, if the current user has access to it.
getLocalTasksWidget :: [(Text, Route App)] -> Handler (Maybe Widget)
getLocalTasksWidget routes = do
    mcurrentRoute <- getCurrentRoute

    -- @todo: Keep only accessible routes.
    return $ Just $ do
        toWidget [whamlet|
            <div class="ui tabular menu">
                $forall (text, route) <- routes
                    <a class="item" :Just route == mcurrentRoute:.active href="@{route}">#{text}
        |]
