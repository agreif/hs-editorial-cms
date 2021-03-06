{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.User where

import Handler.Common
import Handler.Mailer
import Import

import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)

-------------------------------------------------------
-- add user
-------------------------------------------------------

-- gen data add - start
data VAddUser = VAddUser
  { vAddUserIdent :: Text
  , vAddUserEmail :: Text
  , vAddUserIsAdmin :: Bool
  , vAddUserIsEditor :: Bool
  , vAddUserIsReviewer :: Bool
  , vAddUserIsAuthor :: Bool
  }
-- gen data add - end

-- gen get add form - start
getAddUserFormR :: Handler Html
getAddUserFormR = do
  (formWidget, _) <- generateFormPost $ vAddUserForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgUserAddUser}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ AddUserR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

postAddUserR :: Handler Value
postAddUserR = do
  ((result, formWidget), _) <- runFormPost $ vAddUserForm Nothing
  case result of
    FormSuccess vAddUser -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      (passwd, passwdHash) <- liftIO $ generatePassword 32
      urlRenderer <- getUrlRender
      let user = User
            { userIdent = vAddUserIdent vAddUser
            , userPassword = (Just passwdHash)
            , userEmail = vAddUserEmail vAddUser
            , userIsAdmin = vAddUserIsAdmin vAddUser
            , userIsEditor = vAddUserIsEditor vAddUser
            , userIsReviewer = vAddUserIsReviewer vAddUser
            , userIsAuthor = vAddUserIsAuthor vAddUser
            , userVersion = 1
            , userCreatedAt = curTime
            , userCreatedBy = userIdent authUser
            , userUpdatedAt = curTime
            , userUpdatedBy = userIdent authUser
            }
      _ <- runDB $ insert user
      sendPasswordNewAccountMail user passwd
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen add form - start
vAddUserForm :: Maybe User -> Html -> MForm Handler (FormResult VAddUser, Widget)
vAddUserForm maybeUser extra = do
  (identResult, identView) <- mreq textField
    identFs
    (userIdent <$> maybeUser)
  (emailResult, emailView) <- mreq textField
    emailFs
    (userEmail <$> maybeUser)
  (isAdminResult, isAdminView) <- mreq checkBoxField
    isAdminFs
    (userIsAdmin <$> maybeUser)
  (isEditorResult, isEditorView) <- mreq checkBoxField
    isEditorFs
    (userIsEditor <$> maybeUser)
  (isReviewerResult, isReviewerView) <- mreq checkBoxField
    isReviewerFs
    (userIsReviewer <$> maybeUser)
  (isAuthorResult, isAuthorView) <- mreq checkBoxField
    isAuthorFs
    (userIsAuthor <$> maybeUser)
  let vAddUserResult = VAddUser <$> identResult <*> emailResult <*> isAdminResult <*> isEditorResult <*> isReviewerResult <*> isAuthorResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors identView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors identView:.uk-text-danger for=#{fvId identView}>#{fvLabel identView}
      <div .uk-form-controls>
        ^{fvInput identView}
        $maybe err <- fvErrors identView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors emailView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors emailView:.uk-text-danger for=#{fvId emailView}>#{fvLabel emailView}
      <div .uk-form-controls>
        ^{fvInput emailView}
        $maybe err <- fvErrors emailView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors isAdminView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors isAdminView:.uk-text-danger for=#{fvId isAdminView}>#{fvLabel isAdminView}
      <div .uk-form-controls>
        ^{fvInput isAdminView}
        $maybe err <- fvErrors isAdminView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors isEditorView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors isEditorView:.uk-text-danger for=#{fvId isEditorView}>#{fvLabel isEditorView}
      <div .uk-form-controls>
        ^{fvInput isEditorView}
        $maybe err <- fvErrors isEditorView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors isReviewerView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors isReviewerView:.uk-text-danger for=#{fvId isReviewerView}>#{fvLabel isReviewerView}
      <div .uk-form-controls>
        ^{fvInput isReviewerView}
        $maybe err <- fvErrors isReviewerView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors isAuthorView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors isAuthorView:.uk-text-danger for=#{fvId isAuthorView}>#{fvLabel isAuthorView}
      <div .uk-form-controls>
        ^{fvInput isAuthorView}
        $maybe err <- fvErrors isAuthorView
          &nbsp;#{err}
    |]
  return (vAddUserResult, formWidget)
  where
    identFs :: FieldSettings App
    identFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIdent
      , fsTooltip = Nothing
      , fsId = Just "ident"
      , fsName = Just "ident"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    emailFs :: FieldSettings App
    emailFs = FieldSettings
      { fsLabel = SomeMessage MsgUserEmail
      , fsTooltip = Nothing
      , fsId = Just "email"
      , fsName = Just "email"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    isAdminFs :: FieldSettings App
    isAdminFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIsAdmin
      , fsTooltip = Nothing
      , fsId = Just "isAdmin"
      , fsName = Just "isAdmin"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    isEditorFs :: FieldSettings App
    isEditorFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIsEditor
      , fsTooltip = Nothing
      , fsId = Just "isEditor"
      , fsName = Just "isEditor"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    isReviewerFs :: FieldSettings App
    isReviewerFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIsReviewer
      , fsTooltip = Nothing
      , fsId = Just "isReviewer"
      , fsName = Just "isReviewer"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    isAuthorFs :: FieldSettings App
    isAuthorFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIsAuthor
      , fsTooltip = Nothing
      , fsId = Just "isAuthor"
      , fsName = Just "isAuthor"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
-- gen add form - end

-------------------------------------------------------
-- edit user
-------------------------------------------------------

-- gen data edit - start
data VEditUser = VEditUser
  { vEditUserIdent :: Text
  , vEditUserEmail :: Text
  , vEditUserIsAdmin :: Bool
  , vEditUserIsEditor :: Bool
  , vEditUserIsReviewer :: Bool
  , vEditUserIsAuthor :: Bool
  , vEditUserIsResetPassword :: Bool
  , vEditUserVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditUserFormR :: UserId -> Handler Html
getEditUserFormR userId = do
  user <- runDB $ get404 userId
  (formWidget, _) <- generateFormPost $ vEditUserForm (Just user)
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgUserEditUser}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ EditUserR userId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

postEditUserR :: UserId -> Handler Value
postEditUserR userId = do
  ((result, formWidget), _) <- runFormPost $ vEditUserForm Nothing
  case result of
    FormSuccess vEditUser -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      (passwd, passwdHash) <- liftIO $ generatePassword 32
      urlRenderer <- getUrlRender
      let persistFields =
            [ UserIdent =. vEditUserIdent vEditUser
            , UserEmail =. vEditUserEmail vEditUser
            , UserIsAdmin =. vEditUserIsAdmin vEditUser
            , UserIsEditor =. vEditUserIsEditor vEditUser
            , UserIsReviewer =. vEditUserIsReviewer vEditUser
            , UserIsAuthor =. vEditUserIsAuthor vEditUser
            , UserVersion =. vEditUserVersion vEditUser + 1
            , UserUpdatedAt =. curTime
            , UserUpdatedBy =. userIdent authUser
            ]
      let persistFields' = persistFields ++ if vEditUserIsResetPassword vEditUser
                                            then [UserPassword =. (Just passwdHash)]
                                            else []

      updateCount <- runDB $ updateWhereCount [ UserId ==. userId
                                              , UserVersion ==. vEditUserVersion vEditUser
                                              ] persistFields'

      when (vEditUserIsResetPassword vEditUser) $ do
        user' <- runDB $ get404 userId
        sendPasswordResetMail user' passwd
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ AdminR AdminDataR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen edit form - start
vEditUserForm :: Maybe User -> Html -> MForm Handler (FormResult VEditUser, Widget)
vEditUserForm maybeUser extra = do
  (identResult, identView) <- mreq textField
    identFs
    (userIdent <$> maybeUser)
  (emailResult, emailView) <- mreq textField
    emailFs
    (userEmail <$> maybeUser)
  (isAdminResult, isAdminView) <- mreq checkBoxField
    isAdminFs
    (userIsAdmin <$> maybeUser)
  (isEditorResult, isEditorView) <- mreq checkBoxField
    isEditorFs
    (userIsEditor <$> maybeUser)
  (isReviewerResult, isReviewerView) <- mreq checkBoxField
    isReviewerFs
    (userIsReviewer <$> maybeUser)
  (isAuthorResult, isAuthorView) <- mreq checkBoxField
    isAuthorFs
    (userIsAuthor <$> maybeUser)
  (isResetPasswordResult, isResetPasswordView) <- mreq checkBoxField
    isResetPasswordFs
    (Nothing)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (userVersion <$> maybeUser)
  let vEditUserResult = VEditUser <$> identResult <*> emailResult <*> isAdminResult <*> isEditorResult <*> isReviewerResult <*> isAuthorResult <*> isResetPasswordResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors identView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors identView:.uk-text-danger for=#{fvId identView}>#{fvLabel identView}
      <div .uk-form-controls>
        ^{fvInput identView}
        $maybe err <- fvErrors identView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors emailView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors emailView:.uk-text-danger for=#{fvId emailView}>#{fvLabel emailView}
      <div .uk-form-controls>
        ^{fvInput emailView}
        $maybe err <- fvErrors emailView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors isAdminView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors isAdminView:.uk-text-danger for=#{fvId isAdminView}>#{fvLabel isAdminView}
      <div .uk-form-controls>
        ^{fvInput isAdminView}
        $maybe err <- fvErrors isAdminView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors isEditorView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors isEditorView:.uk-text-danger for=#{fvId isEditorView}>#{fvLabel isEditorView}
      <div .uk-form-controls>
        ^{fvInput isEditorView}
        $maybe err <- fvErrors isEditorView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors isReviewerView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors isReviewerView:.uk-text-danger for=#{fvId isReviewerView}>#{fvLabel isReviewerView}
      <div .uk-form-controls>
        ^{fvInput isReviewerView}
        $maybe err <- fvErrors isReviewerView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors isAuthorView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors isAuthorView:.uk-text-danger for=#{fvId isAuthorView}>#{fvLabel isAuthorView}
      <div .uk-form-controls>
        ^{fvInput isAuthorView}
        $maybe err <- fvErrors isAuthorView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors isResetPasswordView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors isResetPasswordView:.uk-text-danger for=#{fvId isResetPasswordView}>#{fvLabel isResetPasswordView}
      <div .uk-form-controls>
        ^{fvInput isResetPasswordView}
        $maybe err <- fvErrors isResetPasswordView
          &nbsp;#{err}
    |]
  return (vEditUserResult, formWidget)
  where
    identFs :: FieldSettings App
    identFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIdent
      , fsTooltip = Nothing
      , fsId = Just "ident"
      , fsName = Just "ident"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    emailFs :: FieldSettings App
    emailFs = FieldSettings
      { fsLabel = SomeMessage MsgUserEmail
      , fsTooltip = Nothing
      , fsId = Just "email"
      , fsName = Just "email"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    isAdminFs :: FieldSettings App
    isAdminFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIsAdmin
      , fsTooltip = Nothing
      , fsId = Just "isAdmin"
      , fsName = Just "isAdmin"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    isEditorFs :: FieldSettings App
    isEditorFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIsEditor
      , fsTooltip = Nothing
      , fsId = Just "isEditor"
      , fsName = Just "isEditor"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    isReviewerFs :: FieldSettings App
    isReviewerFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIsReviewer
      , fsTooltip = Nothing
      , fsId = Just "isReviewer"
      , fsName = Just "isReviewer"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    isAuthorFs :: FieldSettings App
    isAuthorFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIsAuthor
      , fsTooltip = Nothing
      , fsId = Just "isAuthor"
      , fsName = Just "isAuthor"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    isResetPasswordFs :: FieldSettings App
    isResetPasswordFs = FieldSettings
      { fsLabel = SomeMessage MsgUserIsResetPassword
      , fsTooltip = Nothing
      , fsId = Just "isResetPassword"
      , fsName = Just "isResetPassword"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    versionFs :: FieldSettings App
    versionFs = FieldSettings
      { fsLabel = ""
      , fsTooltip = Nothing
      , fsId = Just "version"
      , fsName = Just "version"
      , fsAttrs = []
      }
-- gen edit form - end

-------------------------------------------------------
-- delete user
-------------------------------------------------------

-- gen delete form - start
vDeleteUserForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteUserForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end

-- gen get delete form - start
getDeleteUserFormR :: UserId -> Handler Html
getDeleteUserFormR userId = do
  (formWidget, _) <- generateFormPost $ vDeleteUserForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgUserDeleteUser}
      <form #modal-form .uk-form-horizontal method=post action=@{AdminR $ DeleteUserR userId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete form - start
postDeleteUserR :: UserId -> Handler Value
postDeleteUserR userId = do
  runDB $ delete userId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR }
-- gen post delete form - end
