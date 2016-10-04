module Handler.EditUser where

import Import

getEditUserR :: Text -> Handler Html
getEditUserR ident = do
    (Entity _ user) <- runDB . getBy404 $ UniqueUser ident

    (widget, enctype) <- generateFormPost $ userForm ident user
    defaultLayout $(widgetFile "user-update")

postEditUserR :: Text -> Handler Html
postEditUserR ident = do
    (Entity _ user) <- runDB . getBy404 $ UniqueUser ident
    ((result, widget), enctype) <- runFormPost $ userForm ident user
    case result of
        FormSuccess user' -> do
            _ <- runDB $ insert user'

            setMessage "User saved"
            redirect $ UserR ident
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{EditUserR ident} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]


userForm :: Text -> User -> Form User
userForm ident user = renderSematnicUiDivs $ User
    <$> pure ident
    <*> pure (userEmail user)
    <*> aopt textField "Full name" Nothing -- (userFullName user)
    <*> aopt textareaField "Description" Nothing -- (userDesc user)
    <*> pure (userAdmin user)
    <*> aopt (selectField optionsEnum) (selectSettings "Employment") Nothing -- (userEmployment user)
    <*> pure True
    <*> pure True
    -- <*> aopt checkBoxField "Blocked"  (userBlocked <$> user)
    -- <*> aopt checkBoxField "Public email"  (userEmailPublic <$> user)
    <*> lift (liftIO getCurrentTime)
    where
        selectSettings label =
            FieldSettings
                { fsLabel = label
                , fsTooltip = Nothing
                , fsId = Nothing
                , fsName = Nothing
                , fsAttrs = [("class", "ui fluid dropdown")]
                }
