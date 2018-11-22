{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Handler.Editorsubmission where

import Handler.Common
import Import

import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI
import qualified Database.Esqueleto as E

type Editorsubmission = Submission
type EditorsubmissionId = SubmissionId

editorsubmissionIssueId :: Submission -> IssueId
editorsubmissionIssueId = submissionIssueId

editorsubmissionHeadline :: Submission -> Text
editorsubmissionHeadline = submissionHeadline

editorsubmissionSubline :: Submission -> Text
editorsubmissionSubline = submissionSubline

editorsubmissionText :: Submission -> Textarea
editorsubmissionText = submissionText

editorsubmissionVersion :: Submission -> Int
editorsubmissionVersion = submissionVersion

-- -------------------------------------------------------
-- -- list
-- -------------------------------------------------------

-- getEditorsubmissionListR :: Handler Html
-- getEditorsubmissionListR = defaultLayout $ do
--   toWidget [whamlet|
--                    <body-tag>
--                    <script>
--                      \ riot.compile(function() {
--                      \   bodyTag = riot.mount('body-tag')[0]
--                      \   bodyTag.refreshData("@{EditorR $ EditorsubmissionListDataR}")
--                      \ })
--                    |]

-- getEditorsubmissionListDataR :: Handler Value
-- getEditorsubmissionListDataR = editorsubmissionListPageNumDataR 1

-- postEditorsubmissionListPageNumDataR :: Int -> Handler Value
-- postEditorsubmissionListPageNumDataR pageNum = do
--   urlRenderer <- getUrlRender
--   returnJson $
--     VFormSubmitSuccess
--     { fsSuccessDataJsonUrl = urlRenderer $ EditorR $ EditorsubmissionListPageNumDataR pageNum }

-- getEditorsubmissionListPageNumDataR :: Int -> Handler Value
-- getEditorsubmissionListPageNumDataR = editorsubmissionListPageNumDataR

-- editorsubmissionListPageNumDataR :: Int -> Handler Value
-- editorsubmissionListPageNumDataR pageNum = do
--   Entity _ user <- requireAuth
--   req <- getRequest
--   appName <- runDB $ configAppName
--   urlRenderer <- getUrlRender
--   mainNavItems <- mainNavData user MainNavEditor
--   (jDataEditorsubmissions, jDataPaginationItems) <- editorsubmissionListJDatas pageNum
--   let pages =
--         defaultDataPages
--         { jDataPageEditorsubmissionList =
--             Just $ JDataPageEditorsubmissionList
--             { jDataPageEditorsubmissionListEditorsubmissions = jDataEditorsubmissions
--             , jDataPageEditorsubmissionListAddFormUrl = urlRenderer $ EditorR AddEditorsubmissionFormR
--             , jDataPageEditorsubmissionListPaginationItems = jDataPaginationItems
--             }
--         }
--   msgHome <- localizedMsg MsgGlobalHome
--   msgEditorsubmissions <- localizedMsg MsgEditorsubmissionSubmissions
--   currentLanguage <- getLanguage
--   translation <- getTranslation
--   let currentDataUrl = urlRenderer $ EditorR EditorsubmissionListDataR
--   returnJson JData
--     { jDataAppName = appName
--     , jDataUserIdent = userIdent user
--     , jDataMainNavItems = mainNavItems
--     , jDataSubNavItems = []
--     , jDataPages = pages
--     , jDataHistoryState = Just JDataHistoryState
--       { jDataHistoryStateUrl = urlRenderer $ EditorR EditorsubmissionListR
--       , jDataHistoryStateTitle = msgEditorsubmissions
--       }
--     , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
--     , jDataCsrfToken = reqToken req
--     , jDataBreadcrumbItems = [ JDataBreadcrumbItem
--                                { jDataBreadcrumbItemLabel = msgHome
--                                , jDataBreadcrumbItemDataUrl = urlRenderer $ EcmsR HomeDataR }
--                              , JDataBreadcrumbItem
--                                { jDataBreadcrumbItemLabel = msgEditorsubmissions
--                                , jDataBreadcrumbItemDataUrl = currentDataUrl }
--                              ]
--     , jDataCurrentLanguage = currentLanguage
--     , jDataTranslation = translation
--     , jDataLanguageDeUrl = urlRenderer $ EcmsR $ LanguageDeR currentDataUrl
--     , jDataLanguageEnUrl = urlRenderer $ EcmsR $ LanguageEnR currentDataUrl
--     }

-- editorsubmissionListJDatas :: Int -> Handler ([JDataEditorsubmission], Maybe [JDataPaginationItem])
-- editorsubmissionListJDatas pageNum = do
--   urlRenderer <- getUrlRender
--   rowCount <- runDB $ count ([] :: [Filter Editorsubmission])
--   paginationJDatas <- getPaginationJDatas rowCount editorsubmissionListPageSize pageNum 11 (EditorR . EditorsubmissionListPageNumDataR)
--   editorsubmissionEnts <- runDB loadEditorsubmissionTuples
--   let editorsubmissionJDatas =
--         map (\editorsubmissionEnt@(Entity editorsubmissionId _) ->
--                JDataEditorsubmission
--                { jDataEditorsubmissionEnt = editorsubmissionEnt
--                , jDataEditorsubmissionDetailUrl = urlRenderer $ EditorR $ EditorsubmissionDetailR editorsubmissionId
--                , jDataEditorsubmissionDetailDataUrl = urlRenderer $ EditorR $ EditorsubmissionDetailDataR editorsubmissionId
--                , jDataEditorsubmissionDeleteFormUrl = urlRenderer $ EditorR $ DeleteEditorsubmissionFormR editorsubmissionId
--                }
--             ) editorsubmissionEnts
--   return (editorsubmissionJDatas, paginationJDatas)
--   where
--     loadEditorsubmissionTuples :: YesodDB App [(Entity Editorsubmission)]
--     loadEditorsubmissionTuples = do
--       let pageSize = fromIntegral editorsubmissionListPageSize
--       E.select $ E.from $ \(da) -> do
--         E.orderBy [ E.desc (da E.^. SubmissionId) ]
--         E.offset ((fromIntegral pageNum - 1) * pageSize)
--         E.limit pageSize
--         return (da)

-- editorsubmissionListPageSize :: Int
-- editorsubmissionListPageSize = 50

-------------------------------------------------------
-- detail
-------------------------------------------------------

getEditorsubmissionDetailR :: EditorsubmissionId -> Handler Html
getEditorsubmissionDetailR editorsubmissionId = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{EditorR $ EditorsubmissionDetailDataR editorsubmissionId}")
                     \ })
                   |]

getEditorsubmissionDetailDataR :: EditorsubmissionId -> Handler Value
getEditorsubmissionDetailDataR editorsubmissionId = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  mainNavItems <- mainNavData user MainNavEditor
  editorsubmission <- runDB $ get404 editorsubmissionId
  issue <- runDB $ get404 $ submissionIssueId editorsubmission
  urlRenderer <- getUrlRender
  jDataEditorsubmissionfiles <- editorsubmissionDetailFileJDatas editorsubmissionId
  let pages =
        defaultDataPages
        { jDataPageEditorsubmissionDetail =
            Just $ JDataPageEditorsubmissionDetail
            { jDataPageEditorsubmissionDetailEditorsubmissionEnt = Entity editorsubmissionId editorsubmission
            , jDataPageEditorsubmissionDetailEditorsubmissionEditFormUrl = urlRenderer $ EditorR $ EditEditorsubmissionFormR editorsubmissionId
            , jDataPageEditorsubmissionDetailEditorsubmissionfiles = jDataEditorsubmissionfiles
            , jDataPageEditorsubmissionDetailEditorsubmissionfileAddFormUrl = urlRenderer $ EditorR $ AddEditorsubmissionfileFormR editorsubmissionId
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgEditorsubmission <- localizedMsg MsgEditorsubmissionSubmission
  msgIssues <- localizedMsg MsgIssueIssues
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ EditorR $ EditorsubmissionDetailDataR editorsubmissionId
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ EditorR $ EditorsubmissionDetailR editorsubmissionId
      , jDataHistoryStateTitle = msgEditorsubmission
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
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ EditorR $ IssueDetailDataR $ submissionIssueId editorsubmission }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = submissionHeadline editorsubmission
                               , jDataBreadcrumbItemDataUrl = currentDataUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ EcmsR $ LanguageDeR currentDataUrl
    , jDataLanguageEnUrl = urlRenderer $ EcmsR $ LanguageEnR currentDataUrl
    }

editorsubmissionDetailFileJDatas :: EditorsubmissionId -> Handler [JDataEditorsubmissionfile]
editorsubmissionDetailFileJDatas editorsubmissionId = do
  urlRenderer <- getUrlRender
  submissionfileTuples <- runDB loadSubmissionfileListTuples
  return $ map (\(submissionfileEnt@(Entity submissionfileId _)) ->
                            JDataEditorsubmissionfile
                            { jDataEditorsubmissionfileEnt = submissionfileEnt
                            , jDataEditorsubmissionfileEditFormUrl = urlRenderer $ EditorR $ EditEditorsubmissionfileFormR submissionfileId
                            , jDataEditorsubmissionfileDeleteFormUrl = urlRenderer $ EditorR $ DeleteEditorsubmissionfileFormR submissionfileId
                            , jDataEditorsubmissionfileDownloadUrl = urlRenderer $ EditorR $ DownloadEditorsubmissionfileR submissionfileId
                            }
                         ) submissionfileTuples
  where
    loadSubmissionfileListTuples :: YesodDB App [(Entity Submissionfile)]
    loadSubmissionfileListTuples = do
      submissionfileTuples <- E.select $ E.from $ \cf -> do
        E.orderBy [E.asc (cf E.^. SubmissionfileId)]
        E.where_ (cf E.^. SubmissionfileSubmissionId E.==. E.val editorsubmissionId)
        return (cf)
      return submissionfileTuples



-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddEditorsubmission = VAddEditorsubmission
  { vAddEditorsubmissionHeadline :: Text
  , vAddEditorsubmissionSubline :: Text
  , vAddEditorsubmissionText :: Textarea
  }
-- gen data add - end

-- gen get add form - start
getAddEditorsubmissionFormR :: IssueId -> Handler Html
getAddEditorsubmissionFormR issueId = do
  (formWidget, _) <- generateFormPost $ vAddEditorsubmissionForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgEditorsubmissionAddSubmission}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{EditorR $ AddEditorsubmissionR issueId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

postAddEditorsubmissionR :: IssueId -> Handler Value
postAddEditorsubmissionR issueId = do
  ((result, formWidget), _) <- runFormPost $ vAddEditorsubmissionForm Nothing
  case result of
    FormSuccess vAddEditorsubmission -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender

      let submission =
            Submission
            { submissionIssueId = issueId
            , submissionHeadline = vAddEditorsubmissionHeadline vAddEditorsubmission
            , submissionSubline = vAddEditorsubmissionSubline vAddEditorsubmission
            , submissionText = vAddEditorsubmissionText vAddEditorsubmission
            , submissionVersion = 1
            , submissionCreatedAt = curTime
            , submissionCreatedBy = userIdent authUser
            , submissionUpdatedAt = curTime
            , submissionUpdatedBy = userIdent authUser
            }
      runDB $ do
        _ <- insert submission
        return ()
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EditorR $ IssueDetailDataR issueId }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen add form - start
vAddEditorsubmissionForm :: Maybe Editorsubmission -> Html -> MForm Handler (FormResult VAddEditorsubmission, Widget)
vAddEditorsubmissionForm maybeEditorsubmission extra = do
  (headlineResult, headlineView) <- mreq textField
    headlineFs
    (editorsubmissionHeadline <$> maybeEditorsubmission)
  (sublineResult, sublineView) <- mreq textField
    sublineFs
    (editorsubmissionSubline <$> maybeEditorsubmission)
  (textResult, textView) <- mreq textareaField
    textFs
    (editorsubmissionText <$> maybeEditorsubmission)
  let vAddEditorsubmissionResult = VAddEditorsubmission <$> headlineResult <*> sublineResult <*> textResult
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
    <div .uk-margin-small :not $ null $ fvErrors textView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors textView:.uk-text-danger for=#{fvId textView}>#{fvLabel textView}
      <div .uk-form-controls>
        ^{fvInput textView}
        $maybe err <- fvErrors textView
          &nbsp;#{err}
    |]
  return (vAddEditorsubmissionResult, formWidget)
  where
    headlineFs :: FieldSettings App
    headlineFs = FieldSettings
      { fsLabel = SomeMessage MsgEditorsubmissionHeadline
      , fsTooltip = Nothing
      , fsId = Just "headline"
      , fsName = Just "headline"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    sublineFs :: FieldSettings App
    sublineFs = FieldSettings
      { fsLabel = SomeMessage MsgEditorsubmissionSubline
      , fsTooltip = Nothing
      , fsId = Just "subline"
      , fsName = Just "subline"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    textFs :: FieldSettings App
    textFs = FieldSettings
      { fsLabel = SomeMessage MsgEditorsubmissionText
      , fsTooltip = Nothing
      , fsId = Just "text"
      , fsName = Just "text"
      , fsAttrs = [ ("class","uk-textarea uk-form-small uk-width-5-6"), ("rows","10") ]
      }
-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditEditorsubmission = VEditEditorsubmission
  { vEditEditorsubmissionIssueId :: IssueId
  , vEditEditorsubmissionHeadline :: Text
  , vEditEditorsubmissionSubline :: Text
  , vEditEditorsubmissionText :: Textarea
  , vEditEditorsubmissionVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditEditorsubmissionFormR :: EditorsubmissionId -> Handler Html
getEditEditorsubmissionFormR editorsubmissionId = do
  editorsubmission <- runDB $ get404 editorsubmissionId
  (formWidget, _) <- generateFormPost $ vEditEditorsubmissionForm (Just editorsubmission)
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgEditorsubmissionEditSubmission}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{EditorR $ EditEditorsubmissionR editorsubmissionId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

postEditEditorsubmissionR :: EditorsubmissionId -> Handler Value
postEditEditorsubmissionR editorsubmissionId = do
  ((result, formWidget), _) <- runFormPost $ vEditEditorsubmissionForm Nothing
  case result of
    FormSuccess vEditEditorsubmission -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields =
            [ SubmissionIssueId =. vEditEditorsubmissionIssueId vEditEditorsubmission
            , SubmissionHeadline =. vEditEditorsubmissionHeadline vEditEditorsubmission
            , SubmissionSubline =. vEditEditorsubmissionSubline vEditEditorsubmission
            , SubmissionText =. vEditEditorsubmissionText vEditEditorsubmission
            , SubmissionVersion =. vEditEditorsubmissionVersion vEditEditorsubmission + 1
            , SubmissionUpdatedAt =. curTime
            , SubmissionUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <- updateWhereCount [ SubmissionId ==. editorsubmissionId
                               , SubmissionVersion ==. vEditEditorsubmissionVersion vEditEditorsubmission
                               ] persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EditorR $ EditorsubmissionDetailDataR editorsubmissionId }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ EditorR $ EditorsubmissionDetailDataR editorsubmissionId }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen edit form - start
vEditEditorsubmissionForm :: Maybe Editorsubmission -> Html -> MForm Handler (FormResult VEditEditorsubmission, Widget)
vEditEditorsubmissionForm maybeEditorsubmission extra = do
  (issueIdResult, issueIdView) <- mreq issueSelectField
    issueIdFs
    (editorsubmissionIssueId <$> maybeEditorsubmission)
  (headlineResult, headlineView) <- mreq textField
    headlineFs
    (editorsubmissionHeadline <$> maybeEditorsubmission)
  (sublineResult, sublineView) <- mreq textField
    sublineFs
    (editorsubmissionSubline <$> maybeEditorsubmission)
  (textResult, textView) <- mreq textareaField
    textFs
    (editorsubmissionText <$> maybeEditorsubmission)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (editorsubmissionVersion <$> maybeEditorsubmission)
  let vEditEditorsubmissionResult = VEditEditorsubmission <$> issueIdResult <*> headlineResult <*> sublineResult <*> textResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors issueIdView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors issueIdView:.uk-text-danger for=#{fvId issueIdView}>#{fvLabel issueIdView}
      <div .uk-form-controls>
        ^{fvInput issueIdView}
        $maybe err <- fvErrors issueIdView
          &nbsp;#{err}
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
    <div .uk-margin-small :not $ null $ fvErrors textView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors textView:.uk-text-danger for=#{fvId textView}>#{fvLabel textView}
      <div .uk-form-controls>
        ^{fvInput textView}
        $maybe err <- fvErrors textView
          &nbsp;#{err}
    |]
  return (vEditEditorsubmissionResult, formWidget)
  where
    issueIdFs :: FieldSettings App
    issueIdFs = FieldSettings
      { fsLabel = SomeMessage MsgEditorsubmissionIssueId
      , fsTooltip = Nothing
      , fsId = Just "issueId"
      , fsName = Just "issueId"
      , fsAttrs = [  ]
      }
    headlineFs :: FieldSettings App
    headlineFs = FieldSettings
      { fsLabel = SomeMessage MsgEditorsubmissionHeadline
      , fsTooltip = Nothing
      , fsId = Just "headline"
      , fsName = Just "headline"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    sublineFs :: FieldSettings App
    sublineFs = FieldSettings
      { fsLabel = SomeMessage MsgEditorsubmissionSubline
      , fsTooltip = Nothing
      , fsId = Just "subline"
      , fsName = Just "subline"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    textFs :: FieldSettings App
    textFs = FieldSettings
      { fsLabel = SomeMessage MsgEditorsubmissionText
      , fsTooltip = Nothing
      , fsId = Just "text"
      , fsName = Just "text"
      , fsAttrs = [ ("class","uk-textarea uk-form-small uk-width-5-6"), ("rows","10") ]
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
getDeleteEditorsubmissionFormR :: EditorsubmissionId -> Handler Html
getDeleteEditorsubmissionFormR editorsubmissionId = do
  (formWidget, _) <- generateFormPost $ vDeleteEditorsubmissionForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgEditorsubmissionDeleteSubmission}
      <form #modal-form .uk-form-horizontal method=post action=@{EditorR $ DeleteEditorsubmissionR editorsubmissionId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete form - start
postDeleteEditorsubmissionR :: EditorsubmissionId -> Handler Value
postDeleteEditorsubmissionR editorsubmissionId = do
  editorsubmission <- runDB $ get404 editorsubmissionId
  runDB $ delete editorsubmissionId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EditorR $ IssueDetailDataR $ submissionIssueId editorsubmission }
-- gen post delete form - end

-- gen delete form - start
vDeleteEditorsubmissionForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteEditorsubmissionForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
