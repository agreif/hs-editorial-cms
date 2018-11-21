{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Handler.Authorsubmission where

import Handler.Common
import Import

import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI
import qualified Database.Esqueleto as E

type Authorsubmission = Submission
type AuthorsubmissionId = SubmissionId

authorsubmissionIssueId :: Submission -> IssueId
authorsubmissionIssueId = submissionIssueId

authorsubmissionHeadline :: Submission -> Text
authorsubmissionHeadline = submissionHeadline

authorsubmissionSubline :: Submission -> Text
authorsubmissionSubline = submissionSubline

authorsubmissionText :: Submission -> Textarea
authorsubmissionText = submissionText

authorsubmissionVersion :: Submission -> Int
authorsubmissionVersion = submissionVersion

issueSelectField :: Field Handler (Key Issue)
issueSelectField = do
  selectField $ optionsPersistKey [] [Asc IssueId] issueName

-------------------------------------------------------
-- list
-------------------------------------------------------

getAuthorsubmissionListR :: Handler Html
getAuthorsubmissionListR = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{AuthorR $ AuthorsubmissionListDataR}")
                     \ })
                   |]

getAuthorsubmissionListDataR :: Handler Value
getAuthorsubmissionListDataR = authorsubmissionListPageNumDataR 1

postAuthorsubmissionListPageNumDataR :: Int -> Handler Value
postAuthorsubmissionListPageNumDataR pageNum = do
  urlRenderer <- getUrlRender
  returnJson $
    VFormSubmitSuccess
    { fsSuccessDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionListPageNumDataR pageNum }

getAuthorsubmissionListPageNumDataR :: Int -> Handler Value
getAuthorsubmissionListPageNumDataR = authorsubmissionListPageNumDataR

authorsubmissionListPageNumDataR :: Int -> Handler Value
authorsubmissionListPageNumDataR pageNum = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavAuthor
  (jDataAuthorsubmissions, jDataPaginationItems) <- authorsubmissionListJDatas pageNum
  let pages =
        defaultDataPages
        { jDataPageAuthorsubmissionList =
            Just $ JDataPageAuthorsubmissionList
            { jDataPageAuthorsubmissionListAuthorsubmissions = jDataAuthorsubmissions
            , jDataPageAuthorsubmissionListAddFormUrl = urlRenderer $ AuthorR AddAuthorsubmissionFormR
            , jDataPageAuthorsubmissionListPaginationItems = jDataPaginationItems
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgAuthorsubmissions <- localizedMsg MsgAuthorsubmissionSubmissions
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ AuthorR AuthorsubmissionListDataR
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ AuthorR AuthorsubmissionListR
      , jDataHistoryStateTitle = msgAuthorsubmissions
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ EcmsR HomeDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgAuthorsubmissions
                               , jDataBreadcrumbItemDataUrl = currentDataUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ EcmsR $ LanguageDeR currentDataUrl
    , jDataLanguageEnUrl = urlRenderer $ EcmsR $ LanguageEnR currentDataUrl
    }

authorsubmissionListJDatas :: Int -> Handler ([JDataAuthorsubmission], Maybe [JDataPaginationItem])
authorsubmissionListJDatas pageNum = do
  urlRenderer <- getUrlRender
  rowCount <- runDB $ count ([] :: [Filter Authorsubmission])
  paginationJDatas <- getPaginationJDatas rowCount authorsubmissionListPageSize pageNum 11 (AuthorR . AuthorsubmissionListPageNumDataR)
  authorsubmissionEnts <- runDB loadAuthorsubmissionTuples
  let authorsubmissionJDatas =
        map (\authorsubmissionEnt@(Entity authorsubmissionId _) ->
               JDataAuthorsubmission
               { jDataAuthorsubmissionEnt = authorsubmissionEnt
               , jDataAuthorsubmissionDetailUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailR authorsubmissionId
               , jDataAuthorsubmissionDetailDataUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR authorsubmissionId
               , jDataAuthorsubmissionDeleteFormUrl = urlRenderer $ AuthorR $ DeleteAuthorsubmissionFormR authorsubmissionId
               }
            ) authorsubmissionEnts
  return (authorsubmissionJDatas, paginationJDatas)
  where
    loadAuthorsubmissionTuples :: YesodDB App [(Entity Authorsubmission)]
    loadAuthorsubmissionTuples = do
      let pageSize = fromIntegral authorsubmissionListPageSize
      E.select $ E.from $ \(da) -> do
        E.orderBy [ E.desc (da E.^. SubmissionId) ]
        E.offset ((fromIntegral pageNum - 1) * pageSize)
        E.limit pageSize
        return (da)

authorsubmissionListPageSize :: Int
authorsubmissionListPageSize = 50

-------------------------------------------------------
-- detail
-------------------------------------------------------

getAuthorsubmissionDetailR :: AuthorsubmissionId -> Handler Html
getAuthorsubmissionDetailR authorsubmissionId = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{AuthorR $ AuthorsubmissionDetailDataR authorsubmissionId}")
                     \ })
                   |]

getAuthorsubmissionDetailDataR :: AuthorsubmissionId -> Handler Value
getAuthorsubmissionDetailDataR authorsubmissionId = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  mainNavItems <- mainNavData user MainNavAuthor
  authorsubmission <- runDB $ get404 authorsubmissionId
  urlRenderer <- getUrlRender
  jDataAuthorsubmissionfiles <- authorsubmissionDetailFileJDatas authorsubmissionId
  let pages =
        defaultDataPages
        { jDataPageAuthorsubmissionDetail =
            Just $ JDataPageAuthorsubmissionDetail
            { jDataPageAuthorsubmissionDetailAuthorsubmissionEnt = Entity authorsubmissionId authorsubmission
            , jDataPageAuthorsubmissionDetailAuthorsubmissionEditFormUrl = urlRenderer $ AuthorR $ EditAuthorsubmissionFormR authorsubmissionId
            , jDataPageAuthorsubmissionDetailAuthorsubmissionfiles = jDataAuthorsubmissionfiles
            , jDataPageAuthorsubmissionDetailAuthorsubmissionfileAddFormUrl = urlRenderer $ AuthorR $ AddAuthorsubmissionfileFormR authorsubmissionId
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgAuthorsubmissions <- localizedMsg MsgAuthorsubmissionSubmissions
  msgAuthorsubmission <- localizedMsg MsgAuthorsubmissionSubmission
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR authorsubmissionId
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailR authorsubmissionId
      , jDataHistoryStateTitle = msgAuthorsubmission
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ EcmsR HomeDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgAuthorsubmissions
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ AuthorR AuthorsubmissionListDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = submissionHeadline authorsubmission
                               , jDataBreadcrumbItemDataUrl = currentDataUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ EcmsR $ LanguageDeR currentDataUrl
    , jDataLanguageEnUrl = urlRenderer $ EcmsR $ LanguageEnR currentDataUrl
    }

authorsubmissionDetailFileJDatas :: AuthorsubmissionId -> Handler [JDataAuthorsubmissionfile]
authorsubmissionDetailFileJDatas authorsubmissionId = do
  urlRenderer <- getUrlRender
  submissionfileTuples <- runDB loadSubmissionfileListTuples
  return $ map (\(submissionfileEnt@(Entity submissionfileId _)) ->
                            JDataAuthorsubmissionfile
                            { jDataAuthorsubmissionfileEnt = submissionfileEnt
                            , jDataAuthorsubmissionfileEditFormUrl = urlRenderer $ AuthorR $ EditAuthorsubmissionfileFormR submissionfileId
                            , jDataAuthorsubmissionfileDeleteFormUrl = urlRenderer $ AuthorR $ DeleteAuthorsubmissionfileFormR submissionfileId
                            , jDataAuthorsubmissionfileDownloadUrl = urlRenderer $ AuthorR $ DownloadAuthorsubmissionfileR submissionfileId
                            }
                         ) submissionfileTuples
  where
    loadSubmissionfileListTuples :: YesodDB App [(Entity Submissionfile)]
    loadSubmissionfileListTuples = do
      submissionfileTuples <- E.select $ E.from $ \cf -> do
        E.orderBy [E.asc (cf E.^. SubmissionfileId)]
        E.where_ (cf E.^. SubmissionfileSubmissionId E.==. E.val authorsubmissionId)
        return (cf)
      return submissionfileTuples




-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddAuthorsubmission = VAddAuthorsubmission
  { vAddAuthorsubmissionIssueId :: IssueId
  , vAddAuthorsubmissionHeadline :: Text
  , vAddAuthorsubmissionSubline :: Text
  , vAddAuthorsubmissionText :: Textarea
  }
-- gen data add - end

-- gen get add form - start
getAddAuthorsubmissionFormR :: Handler Html
getAddAuthorsubmissionFormR = do
  (formWidget, _) <- generateFormPost $ vAddAuthorsubmissionForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgAuthorsubmissionAddSubmission}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AuthorR $ AddAuthorsubmissionR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

postAddAuthorsubmissionR :: Handler Value
postAddAuthorsubmissionR = do
  ((result, formWidget), _) <- runFormPost $ vAddAuthorsubmissionForm Nothing
  case result of
    FormSuccess vAddAuthorsubmission -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender

      let submission =
            Submission
            { submissionIssueId = vAddAuthorsubmissionIssueId vAddAuthorsubmission
            , submissionHeadline = vAddAuthorsubmissionHeadline vAddAuthorsubmission
            , submissionSubline = vAddAuthorsubmissionSubline vAddAuthorsubmission
            , submissionText = vAddAuthorsubmissionText vAddAuthorsubmission
            , submissionVersion = 1
            , submissionCreatedAt = curTime
            , submissionCreatedBy = userIdent authUser
            , submissionUpdatedAt = curTime
            , submissionUpdatedBy = userIdent authUser
            }
      runDB $ do
        _ <- insert submission
        return ()
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AuthorR AuthorsubmissionListDataR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen add form - start
vAddAuthorsubmissionForm :: Maybe Authorsubmission -> Html -> MForm Handler (FormResult VAddAuthorsubmission, Widget)
vAddAuthorsubmissionForm maybeAuthorsubmission extra = do
  (issueIdResult, issueIdView) <- mreq issueSelectField
    issueIdFs
    (authorsubmissionIssueId <$> maybeAuthorsubmission)
  (headlineResult, headlineView) <- mreq textField
    headlineFs
    (authorsubmissionHeadline <$> maybeAuthorsubmission)
  (sublineResult, sublineView) <- mreq textField
    sublineFs
    (authorsubmissionSubline <$> maybeAuthorsubmission)
  (textResult, textView) <- mreq textareaField
    textFs
    (authorsubmissionText <$> maybeAuthorsubmission)
  let vAddAuthorsubmissionResult = VAddAuthorsubmission <$> issueIdResult <*> headlineResult <*> sublineResult <*> textResult
  let formWidget = toWidget [whamlet|
    #{extra}
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
  return (vAddAuthorsubmissionResult, formWidget)
  where
    issueIdFs :: FieldSettings App
    issueIdFs = FieldSettings
      { fsLabel = SomeMessage MsgAuthorsubmissionIssueId
      , fsTooltip = Nothing
      , fsId = Just "issueId"
      , fsName = Just "issueId"
      , fsAttrs = [  ]
      }
    headlineFs :: FieldSettings App
    headlineFs = FieldSettings
      { fsLabel = SomeMessage MsgAuthorsubmissionHeadline
      , fsTooltip = Nothing
      , fsId = Just "headline"
      , fsName = Just "headline"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    sublineFs :: FieldSettings App
    sublineFs = FieldSettings
      { fsLabel = SomeMessage MsgAuthorsubmissionSubline
      , fsTooltip = Nothing
      , fsId = Just "subline"
      , fsName = Just "subline"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    textFs :: FieldSettings App
    textFs = FieldSettings
      { fsLabel = SomeMessage MsgAuthorsubmissionText
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
data VEditAuthorsubmission = VEditAuthorsubmission
  { vEditAuthorsubmissionIssueId :: IssueId
  , vEditAuthorsubmissionHeadline :: Text
  , vEditAuthorsubmissionSubline :: Text
  , vEditAuthorsubmissionText :: Textarea
  , vEditAuthorsubmissionVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditAuthorsubmissionFormR :: AuthorsubmissionId -> Handler Html
getEditAuthorsubmissionFormR authorsubmissionId = do
  authorsubmission <- runDB $ get404 authorsubmissionId
  (formWidget, _) <- generateFormPost $ vEditAuthorsubmissionForm (Just authorsubmission)
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgAuthorsubmissionEditSubmission}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AuthorR $ EditAuthorsubmissionR authorsubmissionId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

postEditAuthorsubmissionR :: AuthorsubmissionId -> Handler Value
postEditAuthorsubmissionR authorsubmissionId = do
  ((result, formWidget), _) <- runFormPost $ vEditAuthorsubmissionForm Nothing
  case result of
    FormSuccess vEditAuthorsubmission -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields =
            [ SubmissionHeadline =. vEditAuthorsubmissionHeadline vEditAuthorsubmission
            , SubmissionSubline =. vEditAuthorsubmissionSubline vEditAuthorsubmission
            , SubmissionText =. vEditAuthorsubmissionText vEditAuthorsubmission
            , SubmissionVersion =. vEditAuthorsubmissionVersion vEditAuthorsubmission + 1
            , SubmissionUpdatedAt =. curTime
            , SubmissionUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <- updateWhereCount [ SubmissionId ==. authorsubmissionId
                               , SubmissionVersion ==. vEditAuthorsubmissionVersion vEditAuthorsubmission
                               ] persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR authorsubmissionId }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR authorsubmissionId }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen edit form - start
vEditAuthorsubmissionForm :: Maybe Authorsubmission -> Html -> MForm Handler (FormResult VEditAuthorsubmission, Widget)
vEditAuthorsubmissionForm maybeAuthorsubmission extra = do
  (issueIdResult, issueIdView) <- mreq issueSelectField
    issueIdFs
    (authorsubmissionIssueId <$> maybeAuthorsubmission)
  (headlineResult, headlineView) <- mreq textField
    headlineFs
    (authorsubmissionHeadline <$> maybeAuthorsubmission)
  (sublineResult, sublineView) <- mreq textField
    sublineFs
    (authorsubmissionSubline <$> maybeAuthorsubmission)
  (textResult, textView) <- mreq textareaField
    textFs
    (authorsubmissionText <$> maybeAuthorsubmission)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (authorsubmissionVersion <$> maybeAuthorsubmission)
  let vEditAuthorsubmissionResult = VEditAuthorsubmission <$> issueIdResult <*> headlineResult <*> sublineResult <*> textResult <*> versionResult
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
  return (vEditAuthorsubmissionResult, formWidget)
  where
    issueIdFs :: FieldSettings App
    issueIdFs = FieldSettings
      { fsLabel = SomeMessage MsgAuthorsubmissionIssueId
      , fsTooltip = Nothing
      , fsId = Just "issueId"
      , fsName = Just "issueId"
      , fsAttrs = [  ]
      }
    headlineFs :: FieldSettings App
    headlineFs = FieldSettings
      { fsLabel = SomeMessage MsgAuthorsubmissionHeadline
      , fsTooltip = Nothing
      , fsId = Just "headline"
      , fsName = Just "headline"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    sublineFs :: FieldSettings App
    sublineFs = FieldSettings
      { fsLabel = SomeMessage MsgAuthorsubmissionSubline
      , fsTooltip = Nothing
      , fsId = Just "subline"
      , fsName = Just "subline"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    textFs :: FieldSettings App
    textFs = FieldSettings
      { fsLabel = SomeMessage MsgAuthorsubmissionText
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
getDeleteAuthorsubmissionFormR :: AuthorsubmissionId -> Handler Html
getDeleteAuthorsubmissionFormR authorsubmissionId = do
  (formWidget, _) <- generateFormPost $ vDeleteAuthorsubmissionForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgAuthorsubmissionDeleteSubmission}
      <form #modal-form .uk-form-horizontal method=post action=@{AuthorR $ DeleteAuthorsubmissionR authorsubmissionId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete form - start
postDeleteAuthorsubmissionR :: AuthorsubmissionId -> Handler Value
postDeleteAuthorsubmissionR authorsubmissionId = do
  runDB $ delete authorsubmissionId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionListDataR }
-- gen post delete form - end

-- gen delete form - start
vDeleteAuthorsubmissionForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteAuthorsubmissionForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
