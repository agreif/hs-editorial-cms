{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.RubricType where

import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddRubricType = VAddRubricType
  { vAddRubricTypeName :: Text
  , vAddRubricTypeSortIndex :: Int
  }
-- gen data add - end

-- gen get add form - start
getAddRubricTypeFormR :: Handler Html
getAddRubricTypeFormR = do
  (formWidget, _) <- generateFormPost $ vAddRubricTypeForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgRubricTypeAddRubricType}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ AddRubricTypeR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

-- gen post add form - start
postAddRubricTypeR :: Handler Value
postAddRubricTypeR = do
  ((result, formWidget), _) <- runFormPost $ vAddRubricTypeForm Nothing
  case result of
    FormSuccess vAddRubricType -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let rubricType = RubricType
            {
            rubricTypeName = vAddRubricTypeName vAddRubricType
            , rubricTypeSortIndex = vAddRubricTypeSortIndex vAddRubricType
            , rubricTypeVersion = 1
            , rubricTypeCreatedAt = curTime
            , rubricTypeCreatedBy = userIdent authUser
            , rubricTypeUpdatedAt = curTime
            , rubricTypeUpdatedBy = userIdent authUser
            }
      runDB $ do
        _ <- insert rubricType
        return ()
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }
-- gen post add form - end

-- gen add form - start
vAddRubricTypeForm :: Maybe RubricType -> Html -> MForm Handler (FormResult VAddRubricType, Widget)
vAddRubricTypeForm maybeRubricType extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (rubricTypeName <$> maybeRubricType)
  (sortIndexResult, sortIndexView) <- mreq intField
    sortIndexFs
    (rubricTypeSortIndex <$> maybeRubricType)
  let vAddRubricTypeResult = VAddRubricType <$> nameResult <*> sortIndexResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        $maybe err <- fvErrors nameView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors sortIndexView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors sortIndexView:.uk-text-danger for=#{fvId sortIndexView}>#{fvLabel sortIndexView}
      <div .uk-form-controls>
        ^{fvInput sortIndexView}
        $maybe err <- fvErrors sortIndexView
          &nbsp;#{err}
    |]
  return (vAddRubricTypeResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgRubricTypeName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    sortIndexFs :: FieldSettings App
    sortIndexFs = FieldSettings
      { fsLabel = SomeMessage MsgRubricTypeSortIndex
      , fsTooltip = Nothing
      , fsId = Just "sortIndex"
      , fsName = Just "sortIndex"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
      }
-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditRubricType = VEditRubricType
  { vEditRubricTypeName :: Text
  , vEditRubricTypeSortIndex :: Int
  , vEditRubricTypeVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditRubricTypeFormR :: RubricTypeId -> Handler Html
getEditRubricTypeFormR rubricTypeId = do
  rubricType <- runDB $ get404 rubricTypeId
  (formWidget, _) <- generateFormPost $ vEditRubricTypeForm (Just rubricType)
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgRubricTypeEditRubricType}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ EditRubricTypeR rubricTypeId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

-- gen post edit form - start
postEditRubricTypeR :: RubricTypeId -> Handler Value
postEditRubricTypeR rubricTypeId = do
  ((result, formWidget), _) <- runFormPost $ vEditRubricTypeForm Nothing
  case result of
    FormSuccess vEditRubricType -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields = [
            RubricTypeName =. vEditRubricTypeName vEditRubricType
            , RubricTypeSortIndex =. vEditRubricTypeSortIndex vEditRubricType
            , RubricTypeVersion =. vEditRubricTypeVersion vEditRubricType + 1
            , RubricTypeUpdatedAt =. curTime
            , RubricTypeUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <- updateWhereCount [ RubricTypeId ==. rubricTypeId
                               , RubricTypeVersion ==. vEditRubricTypeVersion vEditRubricType
                               ] persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ AdminR AdminDataR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen post edit form - end

-- gen edit form - start
vEditRubricTypeForm :: Maybe RubricType -> Html -> MForm Handler (FormResult VEditRubricType, Widget)
vEditRubricTypeForm maybeRubricType extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (rubricTypeName <$> maybeRubricType)
  (sortIndexResult, sortIndexView) <- mreq intField
    sortIndexFs
    (rubricTypeSortIndex <$> maybeRubricType)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (rubricTypeVersion <$> maybeRubricType)
  let vEditRubricTypeResult = VEditRubricType <$> nameResult <*> sortIndexResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        $maybe err <- fvErrors nameView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors sortIndexView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors sortIndexView:.uk-text-danger for=#{fvId sortIndexView}>#{fvLabel sortIndexView}
      <div .uk-form-controls>
        ^{fvInput sortIndexView}
        $maybe err <- fvErrors sortIndexView
          &nbsp;#{err}
    |]
  return (vEditRubricTypeResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgRubricTypeName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    sortIndexFs :: FieldSettings App
    sortIndexFs = FieldSettings
      { fsLabel = SomeMessage MsgRubricTypeSortIndex
      , fsTooltip = Nothing
      , fsId = Just "sortIndex"
      , fsName = Just "sortIndex"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
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
-- delete
-------------------------------------------------------

-- gen get delete form - start
getDeleteRubricTypeFormR :: RubricTypeId -> Handler Html
getDeleteRubricTypeFormR rubricTypeId = do
  (formWidget, _) <- generateFormPost $ vDeleteRubricTypeForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgRubricTypeDeleteRubricType}
      <form #modal-form .uk-form-horizontal method=post action=@{AdminR $ DeleteRubricTypeR rubricTypeId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete form - start
postDeleteRubricTypeR :: RubricTypeId -> Handler Value
postDeleteRubricTypeR rubricTypeId = do
  runDB $ delete rubricTypeId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminDataR }
-- gen post delete form - end

-- gen delete form - start
vDeleteRubricTypeForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteRubricTypeForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
