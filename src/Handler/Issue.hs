{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Handler.Issue where

import Handler.Common
import Import

import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI
import qualified Database.Esqueleto as E

-------------------------------------------------------
-- list
-------------------------------------------------------

getIssueListR :: Handler Html
getIssueListR = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{EditorR $ IssueListDataR}")
                     \ })
                   |]

getIssueListDataR :: Handler Value
getIssueListDataR = issueListPageNumDataR 1

postIssueListPageNumDataR :: Int -> Handler Value
postIssueListPageNumDataR pageNum = do
  urlRenderer <- getUrlRender
  returnJson $
    VFormSubmitSuccess
    { fsSuccessDataJsonUrl = urlRenderer $ EditorR $ IssueListPageNumDataR pageNum }

getIssueListPageNumDataR :: Int -> Handler Value
getIssueListPageNumDataR = issueListPageNumDataR

issueListPageNumDataR :: Int -> Handler Value
issueListPageNumDataR pageNum = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavEditor
  (jDataIssues, jDataPaginationItems) <- issueListJDatas pageNum
  let pages =
        defaultDataPages
        { jDataPageIssueList =
            Just $ JDataPageIssueList
            { jDataPageIssueListIssues = jDataIssues
            , jDataPageIssueListAddFormUrl = urlRenderer $ EditorR AddIssueFormR
            , jDataPageIssueListPaginationItems = jDataPaginationItems
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgIssues <- localizedMsg MsgIssueIssues
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ EditorR IssueListDataR
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ EditorR IssueListR
      , jDataHistoryStateTitle = msgIssues
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ EcmsR HomeDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgIssues
                               , jDataBreadcrumbItemDataUrl = currentDataUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ EcmsR $ LanguageDeR currentDataUrl
    , jDataLanguageEnUrl = urlRenderer $ EcmsR $ LanguageEnR currentDataUrl
    }

issueListJDatas :: Int -> Handler ([JDataIssue], Maybe [JDataPaginationItem])
issueListJDatas pageNum = do
  urlRenderer <- getUrlRender
  rowCount <- runDB $ count ([] :: [Filter Issue])
  paginationJDatas <- getPaginationJDatas rowCount issueListPageSize pageNum 11 (EditorR . IssueListPageNumDataR)
  issueEnts <- runDB loadIssueTuples
  let issueJDatas =
        map (\issueEnt@(Entity issueId _) ->
               JDataIssue
               { jDataIssueEnt = issueEnt
               , jDataIssueDetailUrl = urlRenderer $ EditorR $ IssueDetailR issueId
               , jDataIssueDetailDataUrl = urlRenderer $ EditorR $ IssueDetailDataR issueId
               , jDataIssueDeleteFormUrl = urlRenderer $ EditorR $ DeleteIssueFormR issueId
               }
            ) issueEnts
  return (issueJDatas, paginationJDatas)
  where
    loadIssueTuples :: YesodDB App [(Entity Issue)]
    loadIssueTuples = do
      let pageSize = fromIntegral issueListPageSize
      E.select $ E.from $ \(da) -> do
        E.orderBy [ E.desc (da E.^. IssueId) ]
        E.offset ((fromIntegral pageNum - 1) * pageSize)
        E.limit pageSize
        return (da)

issueListPageSize :: Int
issueListPageSize = 5

-------------------------------------------------------
-- detail
-------------------------------------------------------

getIssueDetailR :: IssueId -> Handler Html
getIssueDetailR issueId = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{EditorR $ IssueDetailDataR issueId}")
                     \ })
                   |]

getIssueDetailDataR :: IssueId -> Handler Value
getIssueDetailDataR issueId = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  mainNavItems <- mainNavData user MainNavEditor
  issue <- runDB $ get404 issueId
  jDataEditorsubmissions <- editorsubmissionJDatas issueId
  urlRenderer <- getUrlRender
  let pages =
        defaultDataPages
        { jDataPageIssueDetail =
            Just $ JDataPageIssueDetail
            { jDataPageIssueDetailIssueEnt = Entity issueId issue
            , jDataPageIssueDetailEditorsubmissions = jDataEditorsubmissions
            , jDataPageIssueDetailIssueEditFormUrl = urlRenderer $ EditorR $ EditIssueFormR issueId
            , jDataPageIssueDetailEditorsubmissionAddFormUrl = urlRenderer $ EditorR $ AddEditorsubmissionFormR issueId
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgIssues <- localizedMsg MsgIssueIssues
  msgIssue <- localizedMsg MsgIssueIssue
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ EditorR $ IssueDetailDataR issueId
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ EditorR $ IssueDetailR issueId
      , jDataHistoryStateTitle = msgIssue
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ EcmsR HomeDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgIssues
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ EditorR IssueListDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = issueName issue
                               , jDataBreadcrumbItemDataUrl = currentDataUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ EcmsR $ LanguageDeR currentDataUrl
    , jDataLanguageEnUrl = urlRenderer $ EcmsR $ LanguageEnR currentDataUrl
    }

editorsubmissionJDatas :: IssueId -> Handler [JDataEditorsubmission]
editorsubmissionJDatas issueId = do
  urlRenderer <- getUrlRender
  editorsubmissionTuples <- runDB $ loadEditorsubmissionTuples issueId
  return $ map
    (\(editorsubmissionEnt@(Entity editorsubmissionId _), maybeRubricTypeEnt) ->
       JDataEditorsubmission
       { jDataEditorsubmissionEnt = editorsubmissionEnt
       , jDataInspectionRubricTypeEnt = maybeRubricTypeEnt
       , jDataEditorsubmissionDetailUrl = urlRenderer $ EditorR $ EditorsubmissionDetailR editorsubmissionId
       , jDataEditorsubmissionDetailDataUrl = urlRenderer $ EditorR $ EditorsubmissionDetailDataR editorsubmissionId
       , jDataEditorsubmissionDeleteFormUrl = urlRenderer $ EditorR $ DeleteEditorsubmissionFormR editorsubmissionId
       })
    editorsubmissionTuples

loadEditorsubmissionTuples :: IssueId -> YesodDB App [(Entity Submission, Maybe (Entity RubricType))]
loadEditorsubmissionTuples issueId = do
  E.select $ E.from $ \(s `E.LeftOuterJoin` rt) -> do
    E.on (s E.^. SubmissionRubricTypeId E.==. rt E.?. RubricTypeId)
    E.orderBy [ E.desc (s E.^. SubmissionId) ]
    E.where_ (s E.^. SubmissionIssueId E.==. E.val issueId)
    return (s, rt)

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddIssue = VAddIssue
  { vAddIssueName :: Text
  }
-- gen data add - end

-- gen get add form - start
getAddIssueFormR :: Handler Html
getAddIssueFormR = do
  (formWidget, _) <- generateFormPost $ vAddIssueForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgIssueAddIssue}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{EditorR $ AddIssueR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

-- gen post add - start
postAddIssueR :: Handler Value
postAddIssueR = do
  ((result, formWidget), _) <- runFormPost $ vAddIssueForm Nothing
  case result of
    FormSuccess vAddIssue -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let issue = Issue
            {
            issueName = vAddIssueName vAddIssue
            , issueVersion = 1
            , issueCreatedAt = curTime
            , issueCreatedBy = userIdent authUser
            , issueUpdatedAt = curTime
            , issueUpdatedBy = userIdent authUser
            }
      runDB $ do
        _ <- insert issue
        return ()
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EditorR IssueListDataR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }
-- gen post add - end

-- gen add form - start
vAddIssueForm :: Maybe Issue -> Html -> MForm Handler (FormResult VAddIssue, Widget)
vAddIssueForm maybeIssue extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (issueName <$> maybeIssue)
  let vAddIssueResult = VAddIssue <$> nameResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        $maybe err <- fvErrors nameView
          &nbsp;#{err}
    |]
  return (vAddIssueResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgIssueName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }
-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditIssue = VEditIssue
  { vEditIssueName :: Text
  , vEditIssueVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditIssueFormR :: IssueId -> Handler Html
getEditIssueFormR issueId = do
  issue <- runDB $ get404 issueId
  (formWidget, _) <- generateFormPost $ vEditIssueForm (Just issue)
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgIssueEditIssue}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{EditorR $ EditIssueR issueId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

-- gen post edit - start
postEditIssueR :: IssueId -> Handler Value
postEditIssueR issueId = do
  ((result, formWidget), _) <- runFormPost $ vEditIssueForm Nothing
  case result of
    FormSuccess vEditIssue -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields = [
            IssueName =. vEditIssueName vEditIssue
            , IssueVersion =. vEditIssueVersion vEditIssue + 1
            , IssueUpdatedAt =. curTime
            , IssueUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <- updateWhereCount [ IssueId ==. issueId
                               , IssueVersion ==. vEditIssueVersion vEditIssue
                               ] persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EditorR $ IssueDetailDataR issueId }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ EditorR $ IssueDetailDataR issueId }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen post edit - end

-- gen edit form - start
vEditIssueForm :: Maybe Issue -> Html -> MForm Handler (FormResult VEditIssue, Widget)
vEditIssueForm maybeIssue extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (issueName <$> maybeIssue)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (issueVersion <$> maybeIssue)
  let vEditIssueResult = VEditIssue <$> nameResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        $maybe err <- fvErrors nameView
          &nbsp;#{err}
    |]
  return (vEditIssueResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgIssueName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
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
getDeleteIssueFormR :: IssueId -> Handler Html
getDeleteIssueFormR issueId = do
  (formWidget, _) <- generateFormPost $ vDeleteIssueForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgIssueDeleteIssue}
      <form #modal-form .uk-form-horizontal method=post action=@{EditorR $ DeleteIssueR issueId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete form - start
postDeleteIssueR :: IssueId -> Handler Value
postDeleteIssueR issueId = do
  runDB $ delete issueId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EditorR $ IssueListDataR }
-- gen post delete form - end

-- gen delete form - start
vDeleteIssueForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteIssueForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
