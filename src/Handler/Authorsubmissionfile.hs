{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Authorsubmissionfile where

import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (updateWhereCount)

type Authorsubmissionfile = Submissionfile
type AuthorsubmissionfileId = SubmissionfileId

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddAuthorsubmissionfile = VAddAuthorsubmissionfile
  { vAddAuthorsubmissionfileFile :: FileInfo
  }
-- gen data add - end

-- gen get add form - start
getAddAuthorsubmissionfileFormR :: SubmissionId -> Handler Html
getAddAuthorsubmissionfileFormR submissionId = do
  (formWidget, _) <- generateFormPost $ vAddAuthorsubmissionfileForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgAuthorsubmissionfileAddSubmissionfile}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AuthorR $ AddAuthorsubmissionfileR submissionId}>
        <div #modal-form-widget>
          ^{formWidget}
      <progress id="modal-form-progressbar" class="uk-progress" value="0" max="0">
      |]
-- gen get add form - end

postAddAuthorsubmissionfileR :: SubmissionId -> Handler Value
postAddAuthorsubmissionfileR submissionId = do
  ((result, formWidget), _) <- runFormPost $ vAddAuthorsubmissionfileForm Nothing
  case result of
    FormSuccess (VAddAuthorsubmissionfile { vAddAuthorsubmissionfileFile = fileInfo }) -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      bytes <- fileBytes fileInfo
      _ <- runDB $ do
        rawdataId <- insert $ Rawdata
                     { rawdataBytes = bytes
                     , rawdataVersion = 1
                     , rawdataCreatedAt = curTime
                     , rawdataCreatedBy = userIdent authUser
                     , rawdataUpdatedAt = curTime
                     , rawdataUpdatedBy = userIdent authUser
                     }
        insert $
          Submissionfile
          { submissionfileSubmissionId = submissionId
          , submissionfileRawdataId = rawdataId
          , submissionfileFilename = fileName fileInfo
          , submissionfileMimetype = fileContentType fileInfo
          , submissionfileSize = length bytes
          , submissionfileVersion = 1
          , submissionfileCreatedAt = curTime
          , submissionfileCreatedBy = userIdent authUser
          , submissionfileUpdatedAt = curTime
          , submissionfileUpdatedBy = userIdent authUser
          }
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR submissionId}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

getDownloadAuthorsubmissionfileR :: AuthorsubmissionfileId -> Handler TypedContent
getDownloadAuthorsubmissionfileR authorsubmissionfileId = do
  Submissionfile
    { submissionfileFilename = filename
    , submissionfileMimetype = mimetype
    , submissionfileSize = size
    , submissionfileRawdataId = rawdataId
    } <- runDB $ get404 authorsubmissionfileId
  rawdata <- runDB $ get404 rawdataId
  let bytes = rawdataBytes rawdata
  addHeader "Content-Disposition" $
    T.concat ["attachment; filename=\"", filename, "\""]
  addHeader "Content-Length" (pack $ show size)
  sendResponse (TE.encodeUtf8 mimetype, toContent bytes)

vAddAuthorsubmissionfileForm :: Maybe VAddAuthorsubmissionfile -> Html -> MForm Handler (FormResult VAddAuthorsubmissionfile, Widget)
vAddAuthorsubmissionfileForm maybeVAddAuthorsubmissionfile extra = do
  (fileResult, fileView) <- mreq fileField
    fileFs
    (vAddAuthorsubmissionfileFile <$> maybeVAddAuthorsubmissionfile)
  let vAddAuthorsubmissionfileResult = VAddAuthorsubmissionfile <$> fileResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors fileView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors fileView:.uk-text-danger for=file>File
      <div .uk-form-controls>
        ^{fvInput fileView}
        $maybe err <- fvErrors fileView
          &nbsp;#{err}
    |]
  return (vAddAuthorsubmissionfileResult, formWidget)
  where
    fileFs :: FieldSettings App
    fileFs = FieldSettings
      { fsLabel = "File"
      , fsTooltip = Nothing
      , fsId = Just "file"
      , fsName = Just "file"
      , fsAttrs = []
      }

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditAuthorsubmissionfile = VEditAuthorsubmissionfile
  { vEditAuthorsubmissionfileFile :: FileInfo
  , vEditAuthorsubmissionfileVersion :: Int
  }
-- gen data edit - end

getEditAuthorsubmissionfileFormR :: AuthorsubmissionfileId -> Handler Html
getEditAuthorsubmissionfileFormR authorsubmissionfileId = do
  authorsubmissionfile <- runDB $ get404 authorsubmissionfileId
  (formWidget, _) <- generateFormPost $ vEditAuthorsubmissionfileForm authorsubmissionfile Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>Edit Submission File
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AuthorR $ EditAuthorsubmissionfileR authorsubmissionfileId}>
        <div #modal-form-widget>
          ^{formWidget}
      <progress id="modal-form-progressbar" class="uk-progress" value="0" max="0">
|]

postEditAuthorsubmissionfileR :: AuthorsubmissionfileId -> Handler Value
postEditAuthorsubmissionfileR authorsubmissionfileId = do
  authorsubmissionfile <- runDB $ get404 authorsubmissionfileId
  ((result, formWidget), _) <- runFormPost $ vEditAuthorsubmissionfileForm authorsubmissionfile Nothing
  case result of
    FormSuccess (VEditAuthorsubmissionfile
                 { vEditAuthorsubmissionfileFile = fileInfo
                 , vEditAuthorsubmissionfileVersion = version }) -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      bytes <- fileBytes fileInfo
      let persistFieldsRawdata =
            [ RawdataBytes =. bytes
            , RawdataVersion =. version + 1
            , RawdataUpdatedAt =. curTime
            , RawdataUpdatedBy =. userIdent authUser
            ]
      let persistFieldsAuthorsubmissionfile =
            [ SubmissionfileFilename =. fileName fileInfo
            , SubmissionfileMimetype =. fileContentType fileInfo
            , SubmissionfileSize =. length bytes
            , SubmissionfileVersion =. version + 1
            , SubmissionfileUpdatedAt =. curTime
            , SubmissionfileUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        update (submissionfileRawdataId authorsubmissionfile) persistFieldsRawdata
        updateWhereCount [ SubmissionfileId ==. authorsubmissionfileId
                         , SubmissionfileVersion ==. version
                         ] persistFieldsAuthorsubmissionfile
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR $ submissionfileSubmissionId authorsubmissionfile}
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR $ submissionfileSubmissionId authorsubmissionfile }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

vEditAuthorsubmissionfileForm :: Authorsubmissionfile -> Maybe VEditAuthorsubmissionfile -> Html -> MForm Handler (FormResult VEditAuthorsubmissionfile, Widget)
vEditAuthorsubmissionfileForm authorsubmissionfile maybeVEditAuthorsubmissionfile extra = do
  (fileResult, fileView) <- mreq fileField
    fileFs
    (vEditAuthorsubmissionfileFile <$> maybeVEditAuthorsubmissionfile)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (Just $ submissionfileVersion authorsubmissionfile)
  let vEditAuthorsubmissionfileResult = VEditAuthorsubmissionfile <$> fileResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors fileView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors fileView:.uk-text-danger for=file>File
      <div .uk-form-controls>
        ^{fvInput fileView}
        $maybe err <- fvErrors fileView
          &nbsp;#{err}
    |]
  return (vEditAuthorsubmissionfileResult, formWidget)
  where
    fileFs :: FieldSettings App
    fileFs = FieldSettings
      { fsLabel = "File"
      , fsTooltip = Nothing
      , fsId = Just "file"
      , fsName = Just "file"
      , fsAttrs = []
      }
    versionFs :: FieldSettings App
    versionFs = FieldSettings
      { fsLabel = ""
      , fsTooltip = Nothing
      , fsId = Just "version"
      , fsName = Just "version"
      , fsAttrs = []
      }

-------------------------------------------------------
-- delete
-------------------------------------------------------

-- gen get delete form - start
getDeleteAuthorsubmissionfileFormR :: AuthorsubmissionfileId -> Handler Html
getDeleteAuthorsubmissionfileFormR authorsubmissionfileId = do
  (formWidget, _) <- generateFormPost $ vDeleteAuthorsubmissionfileForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgAuthorsubmissionfileDeleteSubmissionfile}
      <form #modal-form .uk-form-horizontal method=post action=@{AuthorR $ DeleteAuthorsubmissionfileR authorsubmissionfileId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

postDeleteAuthorsubmissionfileR :: AuthorsubmissionfileId -> Handler Value
postDeleteAuthorsubmissionfileR authorsubmissionfileId = do
  submissionId <- runDB $ do
    authorsubmissionfile <- get404 authorsubmissionfileId
    delete authorsubmissionfileId
    delete $ submissionfileRawdataId authorsubmissionfile
    return $ submissionfileSubmissionId authorsubmissionfile
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR submissionId }

-- gen delete form - start
vDeleteAuthorsubmissionfileForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteAuthorsubmissionfileForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
