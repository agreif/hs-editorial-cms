{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Editorsubmissionfile where

import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (updateWhereCount)

type Editorsubmissionfile = Submissionfile
type EditorsubmissionfileId = SubmissionfileId

-- -------------------------------------------------------
-- -- add
-- -------------------------------------------------------

-- -- gen data add - start
-- data VAddEditorsubmissionfile = VAddEditorsubmissionfile
--   { vAddEditorsubmissionfileFile :: FileInfo
--   }
-- -- gen data add - end

-- -- gen get add form - start
-- getAddEditorsubmissionfileFormR :: SubmissionId -> Handler Html
-- getAddEditorsubmissionfileFormR submissionId = do
--   (formWidget, _) <- generateFormPost $ vAddEditorsubmissionfileForm Nothing
--   formLayout $ do
--     toWidget [whamlet|
--       <h1>_{MsgEditorsubmissionfileAddSubmissionfile}
--       <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{EditorR $ AddEditorsubmissionfileR submissionId}>
--         <div #modal-form-widget>
--           ^{formWidget}
--       <progress id="modal-form-progressbar" class="uk-progress" value="0" max="0">
--       |]
-- -- gen get add form - end

-- postAddEditorsubmissionfileR :: SubmissionId -> Handler Value
-- postAddEditorsubmissionfileR submissionId = do
--   ((result, formWidget), _) <- runFormPost $ vAddEditorsubmissionfileForm Nothing
--   case result of
--     FormSuccess (VAddEditorsubmissionfile { vAddEditorsubmissionfileFile = fileInfo }) -> do
--       curTime <- liftIO getCurrentTime
--       Entity _ authUser <- requireAuth
--       urlRenderer <- getUrlRender
--       bytes <- fileBytes fileInfo
--       _ <- runDB $ do
--         rawdataId <- insert $ Rawdata
--                      { rawdataBytes = bytes
--                      , rawdataVersion = 1
--                      , rawdataCreatedAt = curTime
--                      , rawdataCreatedBy = userIdent authUser
--                      , rawdataUpdatedAt = curTime
--                      , rawdataUpdatedBy = userIdent authUser
--                      }
--         insert $
--           Submissionfile
--           { submissionfileSubmissionId = submissionId
--           , submissionfileRawdataId = rawdataId
--           , submissionfileFilename = fileName fileInfo
--           , submissionfileMimetype = fileContentType fileInfo
--           , submissionfileSize = length bytes
--           , submissionfileVersion = 1
--           , submissionfileCreatedAt = curTime
--           , submissionfileCreatedBy = userIdent authUser
--           , submissionfileUpdatedAt = curTime
--           , submissionfileUpdatedBy = userIdent authUser
--           }
--       returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EditorR $ EditorsubmissionDetailDataR submissionId}
--     _ -> do
--       resultHtml <- formLayout [whamlet|^{formWidget}|]
--       returnJson $ VFormSubmitInvalid
--         { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- getDownloadEditorsubmissionfileR :: EditorsubmissionfileId -> Handler TypedContent
-- getDownloadEditorsubmissionfileR editorsubmissionfileId = do
--   Submissionfile
--     { submissionfileFilename = filename
--     , submissionfileMimetype = mimetype
--     , submissionfileSize = size
--     , submissionfileRawdataId = rawdataId
--     } <- runDB $ get404 editorsubmissionfileId
--   rawdata <- runDB $ get404 rawdataId
--   let bytes = rawdataBytes rawdata
--   addHeader "Content-Disposition" $
--     T.concat ["attachment; filename=\"", filename, "\""]
--   addHeader "Content-Length" (pack $ show size)
--   sendResponse (TE.encodeUtf8 mimetype, toContent bytes)

-- vAddEditorsubmissionfileForm :: Maybe VAddEditorsubmissionfile -> Html -> MForm Handler (FormResult VAddEditorsubmissionfile, Widget)
-- vAddEditorsubmissionfileForm maybeVAddEditorsubmissionfile extra = do
--   (fileResult, fileView) <- mreq fileField
--     fileFs
--     (vAddEditorsubmissionfileFile <$> maybeVAddEditorsubmissionfile)
--   let vAddEditorsubmissionfileResult = VAddEditorsubmissionfile <$> fileResult
--   let formWidget = toWidget [whamlet|
--     #{extra}
--     <div .uk-margin-small :not $ null $ fvErrors fileView:.uk-form-danger>
--       <label .uk-form-label :not $ null $ fvErrors fileView:.uk-text-danger for=file>File
--       <div .uk-form-controls>
--         ^{fvInput fileView}
--         $maybe err <- fvErrors fileView
--           &nbsp;#{err}
--     |]
--   return (vAddEditorsubmissionfileResult, formWidget)
--   where
--     fileFs :: FieldSettings App
--     fileFs = FieldSettings
--       { fsLabel = "File"
--       , fsTooltip = Nothing
--       , fsId = Just "file"
--       , fsName = Just "file"
--       , fsAttrs = []
--       }

-- -------------------------------------------------------
-- -- edit
-- -------------------------------------------------------

-- -- gen data edit - start
-- data VEditEditorsubmissionfile = VEditEditorsubmissionfile
--   { vEditEditorsubmissionfileFile :: FileInfo
--   , vEditEditorsubmissionfileVersion :: Int
--   }
-- -- gen data edit - end

-- getEditEditorsubmissionfileFormR :: EditorsubmissionfileId -> Handler Html
-- getEditEditorsubmissionfileFormR editorsubmissionfileId = do
--   editorsubmissionfile <- runDB $ get404 editorsubmissionfileId
--   (formWidget, _) <- generateFormPost $ vEditEditorsubmissionfileForm editorsubmissionfile Nothing
--   formLayout $ do
--     toWidget [whamlet|
--       <h1>Edit Submission File
--       <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{EditorR $ EditEditorsubmissionfileR editorsubmissionfileId}>
--         <div #modal-form-widget>
--           ^{formWidget}
--       <progress id="modal-form-progressbar" class="uk-progress" value="0" max="0">
-- |]

-- postEditEditorsubmissionfileR :: EditorsubmissionfileId -> Handler Value
-- postEditEditorsubmissionfileR editorsubmissionfileId = do
--   editorsubmissionfile <- runDB $ get404 editorsubmissionfileId
--   ((result, formWidget), _) <- runFormPost $ vEditEditorsubmissionfileForm editorsubmissionfile Nothing
--   case result of
--     FormSuccess (VEditEditorsubmissionfile
--                  { vEditEditorsubmissionfileFile = fileInfo
--                  , vEditEditorsubmissionfileVersion = version }) -> do
--       curTime <- liftIO getCurrentTime
--       Entity _ authUser <- requireAuth
--       urlRenderer <- getUrlRender
--       bytes <- fileBytes fileInfo
--       let persistFieldsRawdata =
--             [ RawdataBytes =. bytes
--             , RawdataVersion =. version + 1
--             , RawdataUpdatedAt =. curTime
--             , RawdataUpdatedBy =. userIdent authUser
--             ]
--       let persistFieldsEditorsubmissionfile =
--             [ SubmissionfileFilename =. fileName fileInfo
--             , SubmissionfileMimetype =. fileContentType fileInfo
--             , SubmissionfileSize =. length bytes
--             , SubmissionfileVersion =. version + 1
--             , SubmissionfileUpdatedAt =. curTime
--             , SubmissionfileUpdatedBy =. userIdent authUser
--             ]
--       updateCount <- runDB $ do
--         update (submissionfileRawdataId editorsubmissionfile) persistFieldsRawdata
--         updateWhereCount [ SubmissionfileId ==. editorsubmissionfileId
--                          , SubmissionfileVersion ==. version
--                          ] persistFieldsEditorsubmissionfile
--       if updateCount == 1
--         then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EditorR $ EditorsubmissionDetailDataR $ submissionfileSubmissionId editorsubmissionfile}
--         else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ EditorR $ EditorsubmissionDetailDataR $ submissionfileSubmissionId editorsubmissionfile }
--     _ -> do
--       resultHtml <- formLayout [whamlet|^{formWidget}|]
--       returnJson $ VFormSubmitInvalid
--         { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- vEditEditorsubmissionfileForm :: Editorsubmissionfile -> Maybe VEditEditorsubmissionfile -> Html -> MForm Handler (FormResult VEditEditorsubmissionfile, Widget)
-- vEditEditorsubmissionfileForm editorsubmissionfile maybeVEditEditorsubmissionfile extra = do
--   (fileResult, fileView) <- mreq fileField
--     fileFs
--     (vEditEditorsubmissionfileFile <$> maybeVEditEditorsubmissionfile)
--   (versionResult, versionView) <- mreq hiddenField
--     versionFs
--     (Just $ submissionfileVersion editorsubmissionfile)
--   let vEditEditorsubmissionfileResult = VEditEditorsubmissionfile <$> fileResult <*> versionResult
--   let formWidget = toWidget [whamlet|
--     #{extra}
--     ^{fvInput versionView}
--     <div .uk-margin-small :not $ null $ fvErrors fileView:.uk-form-danger>
--       <label .uk-form-label :not $ null $ fvErrors fileView:.uk-text-danger for=file>File
--       <div .uk-form-controls>
--         ^{fvInput fileView}
--         $maybe err <- fvErrors fileView
--           &nbsp;#{err}
--     |]
--   return (vEditEditorsubmissionfileResult, formWidget)
--   where
--     fileFs :: FieldSettings App
--     fileFs = FieldSettings
--       { fsLabel = "File"
--       , fsTooltip = Nothing
--       , fsId = Just "file"
--       , fsName = Just "file"
--       , fsAttrs = []
--       }
--     versionFs :: FieldSettings App
--     versionFs = FieldSettings
--       { fsLabel = ""
--       , fsTooltip = Nothing
--       , fsId = Just "version"
--       , fsName = Just "version"
--       , fsAttrs = []
--       }

-- -------------------------------------------------------
-- -- delete
-- -------------------------------------------------------

-- -- gen get delete form - start
-- getDeleteEditorsubmissionfileFormR :: EditorsubmissionfileId -> Handler Html
-- getDeleteEditorsubmissionfileFormR editorsubmissionfileId = do
--   (formWidget, _) <- generateFormPost $ vDeleteEditorsubmissionfileForm
--   formLayout $ do
--     toWidget [whamlet|
--       <h1>_{MsgEditorsubmissionfileDeleteSubmissionfile}
--       <form #modal-form .uk-form-horizontal method=post action=@{EditorR $ DeleteEditorsubmissionfileR editorsubmissionfileId}>
--         <div #modal-form-widget>
--           ^{formWidget}
--       |]
-- -- gen get delete form - end

-- postDeleteEditorsubmissionfileR :: EditorsubmissionfileId -> Handler Value
-- postDeleteEditorsubmissionfileR editorsubmissionfileId = do
--   submissionId <- runDB $ do
--     editorsubmissionfile <- get404 editorsubmissionfileId
--     delete editorsubmissionfileId
--     delete $ submissionfileRawdataId editorsubmissionfile
--     return $ submissionfileSubmissionId editorsubmissionfile
--   urlRenderer <- getUrlRender
--   returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ EditorR $ EditorsubmissionDetailDataR submissionId }

-- -- gen delete form - start
-- vDeleteEditorsubmissionfileForm :: Html -> MForm Handler (FormResult (), Widget)
-- vDeleteEditorsubmissionfileForm extra = do
--   let formResult = mempty
--   let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
--   return (formResult, formWidget)
-- -- gen delete form - end
