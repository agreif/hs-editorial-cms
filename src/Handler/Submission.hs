{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Handler.Submission where

import Handler.Common
import Import

import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI

-------------------------------------------------------
-- list
-------------------------------------------------------

getSubmissionListR :: Handler Html
getSubmissionListR = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{EcmsR $ SubmissionListPageDataJsonR}")
                     \ })
                   |]

getSubmissionListPageDataJsonR :: Handler Value
getSubmissionListPageDataJsonR = submissionListPageNumPageDataJsonR 1

postSubmissionListPageNumPageDataJsonR :: Int -> Handler Value
postSubmissionListPageNumPageDataJsonR pageNum = do
  urlRenderer <- getUrlRender
  returnJson $
    VFormSubmitSuccess
    { fsSuccessDataJsonUrl = urlRenderer $ EcmsR $ SubmissionListPageNumPageDataJsonR pageNum }

getSubmissionListPageNumPageDataJsonR :: Int -> Handler Value
getSubmissionListPageNumPageDataJsonR = submissionListPageNumPageDataJsonR

submissionListPageNumPageDataJsonR :: Int -> Handler Value
submissionListPageNumPageDataJsonR pageNum = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavSubmission
  (jDataSubmissions, jDataPaginationItems) <- submissionListJDatas pageNum
  let pages =
        defaultDataPages
        { jDataPageSubmissionList =
            Just $ JDataPageSubmissionList
            { jDataPageSubmissionListSubmissions = jDataSubmissions
            , jDataPageSubmissionListAddFormUrl = urlRenderer $ EcmsR AddSubmissionFormR
            , jDataPageSubmissionListPaginationItems = jDataPaginationItems
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgSubmissions <- localizedMsg MsgSubmissionSubmissions
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentPageDataJsonUrl = urlRenderer $ EcmsR SubmissionListPageDataJsonR
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ EcmsR SubmissionListR
      , jDataHistoryStateTitle = msgSubmissions
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ EcmsR HomePageDataJsonR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgSubmissions
                               , jDataBreadcrumbItemDataUrl = currentPageDataJsonUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ EcmsR $ LanguageDeR currentPageDataJsonUrl
    , jDataLanguageEnUrl = urlRenderer $ EcmsR $ LanguageEnR currentPageDataJsonUrl
    }

submissionListJDatas :: Int -> Handler ([JDataSubmission], Maybe [JDataPaginationItem])
submissionListJDatas pageNum = do
  urlRenderer <- getUrlRender
  rowCount <- runDB $ count ([] :: [Filter Submission])
  paginationJDatas <- getPaginationJDatas rowCount submissionListPageSize pageNum 11 (EcmsR . SubmissionListPageNumPageDataJsonR)
  submissionEnts <- runDB $ selectList [] [Asc SubmissionId]
  let submissionJDatas =
        map (\submissionEnt@(Entity submissionId _) ->
               JDataSubmission
               { jDataSubmissionEnt = submissionEnt
               , jDataSubmissionDetailUrl = urlRenderer $ EcmsR $ SubmissionDetailR submissionId
               , jDataSubmissionDetailDataUrl = urlRenderer $ EcmsR $ SubmissionDetailPageDataJsonR submissionId
               , jDataSubmissionDeleteFormUrl = urlRenderer $ EcmsR $ DeleteSubmissionFormR submissionId
               }
            ) submissionEnts
  return (submissionJDatas, paginationJDatas)

submissionListPageSize :: Int
submissionListPageSize = 50

-------------------------------------------------------
-- detail
-------------------------------------------------------

getSubmissionDetailR :: SubmissionId -> Handler Html
getSubmissionDetailR submissionId = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{EcmsR $ SubmissionDetailPageDataJsonR submissionId}")
                     \ })
                   |]

getSubmissionDetailPageDataJsonR :: SubmissionId -> Handler Value
getSubmissionDetailPageDataJsonR submissionId = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  mainNavItems <- mainNavData user MainNavSubmission
  submission <- runDB $ get404 submissionId
  urlRenderer <- getUrlRender
  let pages =
        defaultDataPages
        { jDataPageSubmissionDetail =
            Just $ JDataPageSubmissionDetail
            { jDataPageSubmissionDetailSubmissionEnt = Entity submissionId submission
            , jDataPageSubmissionDetailSubmissionEditFormUrl = urlRenderer $ EcmsR $ EditSubmissionFormR submissionId
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgSubmissions <- localizedMsg MsgSubmissionSubmissions
  msgSubmission <- localizedMsg MsgSubmissionSubmission
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentPageDataJsonUrl = urlRenderer $ EcmsR $ SubmissionDetailPageDataJsonR submissionId
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ EcmsR $ SubmissionDetailR submissionId
      , jDataHistoryStateTitle = msgSubmission
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ EcmsR HomePageDataJsonR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgSubmissions
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ EcmsR SubmissionListPageDataJsonR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = submissionHeadline submission
                               , jDataBreadcrumbItemDataUrl = currentPageDataJsonUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ EcmsR $ LanguageDeR currentPageDataJsonUrl
    , jDataLanguageEnUrl = urlRenderer $ EcmsR $ LanguageEnR currentPageDataJsonUrl
    }





-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddSubmission = VAddSubmission
  { vAddSubmissionHeadline :: Text
  , vAddSubmissionSubline :: Text
  }
-- gen data add - end

-- gen get add form - start
getAddSubmissionFormR :: Handler Html
getAddSubmissionFormR = do
  (formWidget, _) <- generateFormPost $ vAddSubmissionForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgSubmissionAddSubmission}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{EcmsR $ AddSubmissionR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

-- gen post add - start
postAddSubmissionR :: Handler Value
postAddSubmissionR = do
  ((result, formWidget), _) <- runFormPost $ vAddSubmissionForm Nothing
  case result of
    FormSuccess vAddSubmission -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let submission = Submission
            {
            submissionHeadline = vAddSubmissionHeadline vAddSubmission
            , submissionSubline = vAddSubmissionSubline vAddSubmission
            , submissionVersion = 1
            , submissionCreatedAt = curTime
            , submissionCreatedBy = userIdent authUser
            , submissionUpdatedAt = curTime
            , submissionUpdatedBy = userIdent authUser
            }
      runDB $ do
        _ <- insert submission
        return ()
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EcmsR SubmissionListPageDataJsonR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }
-- gen post add - end

-- gen add form - start
vAddSubmissionForm :: Maybe Submission -> Html -> MForm Handler (FormResult VAddSubmission, Widget)
vAddSubmissionForm maybeSubmission extra = do
  (headlineResult, headlineView) <- mreq textField
    headlineFs
    (submissionHeadline <$> maybeSubmission)
  (sublineResult, sublineView) <- mreq textField
    sublineFs
    (submissionSubline <$> maybeSubmission)
  let vAddSubmissionResult = VAddSubmission <$> headlineResult <*> sublineResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors headlineView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors headlineView:.uk-text-danger for=#{fvId headlineView}>#{fvLabel headlineView}
      <div .uk-form-controls>
        ^{fvInput headlineView}
        $maybe err <- fvErrors headlineView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors sublineView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors sublineView:.uk-text-danger for=#{fvId sublineView}>#{fvLabel sublineView}
      <div .uk-form-controls>
        ^{fvInput sublineView}
        $maybe err <- fvErrors sublineView
          &nbsp;#{err}
    |]
  return (vAddSubmissionResult, formWidget)
  where
    headlineFs :: FieldSettings App
    headlineFs = FieldSettings
      { fsLabel = SomeMessage MsgSubmissionHeadline
      , fsTooltip = Nothing
      , fsId = Just "headline"
      , fsName = Just "headline"
      , fsAttrs = [  ]
      }
    sublineFs :: FieldSettings App
    sublineFs = FieldSettings
      { fsLabel = SomeMessage MsgSubmissionSubline
      , fsTooltip = Nothing
      , fsId = Just "subline"
      , fsName = Just "subline"
      , fsAttrs = [  ]
      }
-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditSubmission = VEditSubmission
  { vEditSubmissionHeadline :: Text
  , vEditSubmissionSubline :: Text
  , vEditSubmissionVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditSubmissionFormR :: SubmissionId -> Handler Html
getEditSubmissionFormR submissionId = do
  submission <- runDB $ get404 submissionId
  (formWidget, _) <- generateFormPost $ vEditSubmissionForm (Just submission)
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgSubmissionEditSubmission}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{EcmsR $ EditSubmissionR submissionId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

-- gen post edit - start
postEditSubmissionR :: SubmissionId -> Handler Value
postEditSubmissionR submissionId = do
  ((result, formWidget), _) <- runFormPost $ vEditSubmissionForm Nothing
  case result of
    FormSuccess vEditSubmission -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields = [
            SubmissionHeadline =. vEditSubmissionHeadline vEditSubmission
            , SubmissionSubline =. vEditSubmissionSubline vEditSubmission
            , SubmissionVersion =. vEditSubmissionVersion vEditSubmission + 1
            , SubmissionUpdatedAt =. curTime
            , SubmissionUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <- updateWhereCount [ SubmissionId ==. submissionId
                               , SubmissionVersion ==. vEditSubmissionVersion vEditSubmission
                               ] persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EcmsR $ SubmissionDetailPageDataJsonR submissionId }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ EcmsR $ SubmissionDetailPageDataJsonR submissionId }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen post edit - end

-- gen edit form - start
vEditSubmissionForm :: Maybe Submission -> Html -> MForm Handler (FormResult VEditSubmission, Widget)
vEditSubmissionForm maybeSubmission extra = do
  (headlineResult, headlineView) <- mreq textField
    headlineFs
    (submissionHeadline <$> maybeSubmission)
  (sublineResult, sublineView) <- mreq textField
    sublineFs
    (submissionSubline <$> maybeSubmission)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (submissionVersion <$> maybeSubmission)
  let vEditSubmissionResult = VEditSubmission <$> headlineResult <*> sublineResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors headlineView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors headlineView:.uk-text-danger for=#{fvId headlineView}>#{fvLabel headlineView}
      <div .uk-form-controls>
        ^{fvInput headlineView}
        $maybe err <- fvErrors headlineView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors sublineView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors sublineView:.uk-text-danger for=#{fvId sublineView}>#{fvLabel sublineView}
      <div .uk-form-controls>
        ^{fvInput sublineView}
        $maybe err <- fvErrors sublineView
          &nbsp;#{err}
    |]
  return (vEditSubmissionResult, formWidget)
  where
    headlineFs :: FieldSettings App
    headlineFs = FieldSettings
      { fsLabel = SomeMessage MsgSubmissionHeadline
      , fsTooltip = Nothing
      , fsId = Just "headline"
      , fsName = Just "headline"
      , fsAttrs = [  ]
      }
    sublineFs :: FieldSettings App
    sublineFs = FieldSettings
      { fsLabel = SomeMessage MsgSubmissionSubline
      , fsTooltip = Nothing
      , fsId = Just "subline"
      , fsName = Just "subline"
      , fsAttrs = [  ]
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
getDeleteSubmissionFormR :: SubmissionId -> Handler Html
getDeleteSubmissionFormR submissionId = do
  (formWidget, _) <- generateFormPost $ vDeleteSubmissionForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgSubmissionDeleteSubmission}
      <form #modal-form .uk-form-horizontal method=post action=@{EcmsR $ DeleteSubmissionR submissionId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete form - start
postDeleteSubmissionR :: SubmissionId -> Handler Value
postDeleteSubmissionR submissionId = do
  runDB $ delete submissionId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EcmsR $ SubmissionListPageDataJsonR }
-- gen post delete form - end

-- gen delete form - start
vDeleteSubmissionForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteSubmissionForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
