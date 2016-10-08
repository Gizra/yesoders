module Utils.LocalTasks
    (getLocalTasksWidget) where

import Import

-- | Get the flag widget, if the current user has access to it.
getLocalTasksWidget :: [(Text, Route App)] -> Handler (Maybe Widget)
getLocalTasksWidget routes = do
    mcurrentRoute <- getCurrentRoute

    -- Keep only accessible routes.
    accessibleRoutes <- getAccessibleRoutes routes
    if null accessibleRoutes
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
        isAuth <- isAuthorized (snd x) False
        result <- getAccessibleRoutes xs
        return $ case isAuth of
            Authorized -> x : result
            _ -> result
