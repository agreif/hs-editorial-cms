{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Submissionfile where

import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (updateWhereCount)

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddSubmissionfile = VAddSubmissionfile
  { vAddSubmissionfileFile :: FileInfo
  }
-- gen data add - end

-- gen get add form - start
getAddSubmissionfileFormR :: SubmissionId -> Handler Html
getAddSubmissionfileFormR submissionId = do
  (formWidget, _) <- generateFormPost $ vAddSubmissionfileForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgSubmissionfileAddSubmissionfile}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AuthorR $ AddSubmissionfileR submissionId}>
        <div #modal-form-widget>
          ^{formWidget}
      <progress id="modal-form-progressbar" class="uk-progress" value="0" max="0">
      |]
-- gen get add form - end

postAddSubmissionfileR :: SubmissionId -> Handler Value
postAddSubmissionfileR submissionId = do
  ((result, formWidget), _) <- runFormPost $ vAddSubmissionfileForm Nothing
  case result of
    FormSuccess (VAddSubmissionfile { vAddSubmissionfileFile = fileInfo }) -> do
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
        insert $ Submissionfile
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

getDownloadSubmissionfileR :: SubmissionfileId -> Handler TypedContent
getDownloadSubmissionfileR submissionfileId = do
  Submissionfile { submissionfileFilename = filename
               , submissionfileMimetype = mimetype
               , submissionfileSize = size
               , submissionfileRawdataId = rawdataId
               } <- runDB $ get404 submissionfileId
  rawdata <- runDB $ get404 rawdataId
  let bytes = rawdataBytes rawdata
  addHeader "Content-Disposition" $
    T.concat ["attachment; filename=\"", filename, "\""]
  addHeader "Content-Length" (pack $ show size)
  sendResponse (TE.encodeUtf8 mimetype, toContent bytes)

vAddSubmissionfileForm :: Maybe VAddSubmissionfile -> Html -> MForm Handler (FormResult VAddSubmissionfile, Widget)
vAddSubmissionfileForm maybeVAddSubmissionfile extra = do
  (fileResult, fileView) <- mreq fileField
    fileFs
    (vAddSubmissionfileFile <$> maybeVAddSubmissionfile)
  let vAddSubmissionfileResult = VAddSubmissionfile <$> fileResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors fileView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors fileView:.uk-text-danger for=file>File
      <div .uk-form-controls>
        ^{fvInput fileView}
        $maybe err <- fvErrors fileView
          &nbsp;#{err}
    |]
  return (vAddSubmissionfileResult, formWidget)
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
data VEditSubmissionfile = VEditSubmissionfile
  { vEditSubmissionfileFile :: FileInfo
  , vEditSubmissionfileVersion :: Int
  }
-- gen data edit - end

getEditSubmissionfileFormR :: SubmissionfileId -> Handler Html
getEditSubmissionfileFormR submissionfileId = do
  submissionfile <- runDB $ get404 submissionfileId
  (formWidget, _) <- generateFormPost $ vEditSubmissionfileForm submissionfile Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>Edit Submission File
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AuthorR $ EditSubmissionfileR submissionfileId}>
        <div #modal-form-widget>
          ^{formWidget}
      <progress id="modal-form-progressbar" class="uk-progress" value="0" max="0">
|]

postEditSubmissionfileR :: SubmissionfileId -> Handler Value
postEditSubmissionfileR submissionfileId = do
  submissionfile <- runDB $ get404 submissionfileId
  ((result, formWidget), _) <- runFormPost $ vEditSubmissionfileForm submissionfile Nothing
  case result of
    FormSuccess (VEditSubmissionfile
                 { vEditSubmissionfileFile = fileInfo
                 , vEditSubmissionfileVersion = version }) -> do
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
      let persistFieldsSubmissionfile =
            [ SubmissionfileFilename =. fileName fileInfo
            , SubmissionfileMimetype =. fileContentType fileInfo
            , SubmissionfileSize =. length bytes
            , SubmissionfileVersion =. version + 1
            , SubmissionfileUpdatedAt =. curTime
            , SubmissionfileUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        update (submissionfileRawdataId submissionfile) persistFieldsRawdata
        updateWhereCount [ SubmissionfileId ==. submissionfileId
                         , SubmissionfileVersion ==. version
                         ] persistFieldsSubmissionfile
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR $ submissionfileSubmissionId submissionfile}
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR $ submissionfileSubmissionId submissionfile }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

vEditSubmissionfileForm :: Submissionfile -> Maybe VEditSubmissionfile -> Html -> MForm Handler (FormResult VEditSubmissionfile, Widget)
vEditSubmissionfileForm submissionfile maybeVEditSubmissionfile extra = do
  (fileResult, fileView) <- mreq fileField
    fileFs
    (vEditSubmissionfileFile <$> maybeVEditSubmissionfile)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (Just $ submissionfileVersion submissionfile)
  let vEditSubmissionfileResult = VEditSubmissionfile <$> fileResult <*> versionResult
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
  return (vEditSubmissionfileResult, formWidget)
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
getDeleteSubmissionfileFormR :: SubmissionfileId -> Handler Html
getDeleteSubmissionfileFormR submissionfileId = do
  (formWidget, _) <- generateFormPost $ vDeleteSubmissionfileForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgSubmissionfileDeleteSubmissionfile}
      <form #modal-form .uk-form-horizontal method=post action=@{AuthorR $ DeleteSubmissionfileR submissionfileId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

postDeleteSubmissionfileR :: SubmissionfileId -> Handler Value
postDeleteSubmissionfileR submissionfileId = do
  submissionId <- runDB $ do
    submissionfile <- get404 submissionfileId
    delete submissionfileId
    delete $ submissionfileRawdataId submissionfile
    return $ submissionfileSubmissionId submissionfile
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AuthorR $ AuthorsubmissionDetailDataR submissionId }

-- gen delete form - start
vDeleteSubmissionfileForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteSubmissionfileForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
