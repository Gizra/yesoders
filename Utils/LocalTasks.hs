module Utils.LocalTasks
    (getLocalTasksWidget) where

import Import

-- | Get the flag widget, if the current user has access to it.
getLocalTasksWidget :: [(Text, Route App)] -> Handler (Maybe Widget)
getLocalTasksWidget routes = do
    mcurrentRoute <- getCurrentRoute

    -- Keep only accessible routes.
    accessibleRoutes <- getAccessibleRoutes routes
    if length accessibleRoutes <= 1
        -- There is no accessible route, or there is only a single allowed
        -- accessible item, so no need it show the tabs, as it's the only
        -- option.
        then return Nothing
        else return $ Just $ do
            toWidget [whamlet|
                <div class="ui tabular menu">
                    $forall (text, route) <- accessibleRoutes
                        <a class="item" :Just route == mcurrentRoute:.active href="@{route}">#{text}
            |]

getAccessibleRoutes :: [(Text, Route App)] -> Handler [(Text, Route App)]
getAccessibleRoutes [] = return []
getAccessibleRoutes (x : xs) = do
        writeRequest <- isWriteRequest (snd x)
        auth <- isAuthorized (snd x) writeRequest
        result <- getAccessibleRoutes xs
        return $ case auth of
            Authorized -> x : result
            _ -> result
