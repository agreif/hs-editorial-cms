{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}

module I18n where

import ClassyPrelude.Yesod

-- gen i18n - start
data AppMessage =
  MsgGlobalHome
  | MsgGlobalAdmin
  | MsgGlobalLogout
  | MsgGlobalLanguage
  | MsgGlobalMyProfile
  | MsgGlobalEditMyProfile
  | MsgGlobalReallyDelete
  | MsgGlobalCancel
  | MsgGlobalEditor
  | MsgGlobalReviewer
  | MsgGlobalAuthor
  | MsgUserIdent
  | MsgUserPassword
  | MsgUserEmail
  | MsgUserIsAdmin
  | MsgUserIsEditor
  | MsgUserIsReviewer
  | MsgUserIsAuthor
  | MsgUserIsResetPassword
  | MsgConfigCode
  | MsgConfigStringValue
  | MsgConfigIntValue
  | MsgConfigDoubleValue
  | MsgConfigBoolValue
  | MsgRawdataBytes
  | MsgSubmissionIssueId
  | MsgSubmissionRubricTypeId
  | MsgSubmissionHeadline
  | MsgSubmissionSubline
  | MsgSubmissionText
  | MsgSubmissionfileSubmissionId
  | MsgSubmissionfileRawdataId
  | MsgSubmissionfileFilename
  | MsgSubmissionfileMimetype
  | MsgSubmissionfileSize
  | MsgAuthorsubmissionIssueId
  | MsgAuthorsubmissionHeadline
  | MsgAuthorsubmissionSubline
  | MsgAuthorsubmissionText
  | MsgAuthorsubmissionfileSubmissionId
  | MsgAuthorsubmissionfileRawdataId
  | MsgAuthorsubmissionfileFilename
  | MsgAuthorsubmissionfileMimetype
  | MsgAuthorsubmissionfileSize
  | MsgAuthorsubmissionfileFile
  | MsgIssueName
  | MsgEditorsubmissionIssueId
  | MsgEditorsubmissionRubricTypeId
  | MsgEditorsubmissionHeadline
  | MsgEditorsubmissionSubline
  | MsgEditorsubmissionText
  | MsgEditorsubmissionfileSubmissionId
  | MsgEditorsubmissionfileRawdataId
  | MsgEditorsubmissionfileFilename
  | MsgEditorsubmissionfileMimetype
  | MsgEditorsubmissionfileSize
  | MsgEditorsubmissionfileFile
  | MsgRubricTypeName
  | MsgRubricTypeSortIndex
  | MsgUserUsers
  | MsgUserAddUser
  | MsgUserEditUser
  | MsgUserDeleteUser
  | MsgConfigConfigurations
  | MsgConfigEditConfig
  | MsgAuthorsubmissionSubmission
  | MsgAuthorsubmissionSubmissions
  | MsgAuthorsubmissionAddSubmission
  | MsgAuthorsubmissionEditSubmission
  | MsgAuthorsubmissionDeleteSubmission
  | MsgAuthorsubmissionfileSubmissionfiles
  | MsgAuthorsubmissionfileAddSubmissionfile
  | MsgAuthorsubmissionfileEditSubmissionfile
  | MsgAuthorsubmissionfileDeleteSubmissionfile
  | MsgAuthorsubmissionfileDownloadSubmissionfile
  | MsgIssueIssue
  | MsgIssueIssues
  | MsgIssueAddIssue
  | MsgIssueEditIssue
  | MsgIssueDeleteIssue
  | MsgEditorsubmissionSubmission
  | MsgEditorsubmissionSubmissions
  | MsgEditorsubmissionAddSubmission
  | MsgEditorsubmissionEditSubmission
  | MsgEditorsubmissionDeleteSubmission
  | MsgEditorsubmissionfileSubmissionfiles
  | MsgEditorsubmissionfileAddSubmissionfile
  | MsgEditorsubmissionfileEditSubmissionfile
  | MsgEditorsubmissionfileDeleteSubmissionfile
  | MsgEditorsubmissionfileDownloadSubmissionfile
  | MsgRubricTypeRubric
  | MsgRubricTypeRubricTypes
  | MsgRubricTypeAddRubricType
  | MsgRubricTypeDeleteRubricType
  | MsgRubricTypeEditRubricType
  | MsgTestmailEmail
  | MsgTestmailTestMail
  | MsgTestmailSendTestMail

renderMessageGerman :: AppMessage -> Text
renderMessageGerman MsgGlobalHome = "Home"
renderMessageGerman MsgGlobalAdmin = "Admin"
renderMessageGerman MsgGlobalLogout = "Logout"
renderMessageGerman MsgGlobalLanguage = "Sprache"
renderMessageGerman MsgGlobalMyProfile = "Mein Profil"
renderMessageGerman MsgGlobalEditMyProfile = "Mein Profil bearbeiten"
renderMessageGerman MsgGlobalReallyDelete = "Möchten sie wirklich löschen?"
renderMessageGerman MsgGlobalCancel = "Abbrechen"
renderMessageGerman MsgGlobalEditor = "Redakteur"
renderMessageGerman MsgGlobalReviewer = "Reviewer"
renderMessageGerman MsgGlobalAuthor = "Autor"
renderMessageGerman MsgUserIdent = "Login"
renderMessageGerman MsgUserPassword = "Passwort"
renderMessageGerman MsgUserEmail = "Email"
renderMessageGerman MsgUserIsAdmin = "Ist Admin?"
renderMessageGerman MsgUserIsEditor = "Ist Redaktuer?"
renderMessageGerman MsgUserIsReviewer = "Ist Gutachter?"
renderMessageGerman MsgUserIsAuthor = "Ist Autor?"
renderMessageGerman MsgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)"
renderMessageGerman MsgConfigCode = "Code"
renderMessageGerman MsgConfigStringValue = "String-Wert"
renderMessageGerman MsgConfigIntValue = "Integer-Wert"
renderMessageGerman MsgConfigDoubleValue = "Double-Wert"
renderMessageGerman MsgConfigBoolValue = "Boolean-Wert"
renderMessageGerman MsgRawdataBytes = "Bytes"
renderMessageGerman MsgSubmissionIssueId = ""
renderMessageGerman MsgSubmissionRubricTypeId = ""
renderMessageGerman MsgSubmissionHeadline = ""
renderMessageGerman MsgSubmissionSubline = ""
renderMessageGerman MsgSubmissionText = ""
renderMessageGerman MsgSubmissionfileSubmissionId = ""
renderMessageGerman MsgSubmissionfileRawdataId = ""
renderMessageGerman MsgSubmissionfileFilename = "Dateiname"
renderMessageGerman MsgSubmissionfileMimetype = "MIME Type"
renderMessageGerman MsgSubmissionfileSize = "Groesse"
renderMessageGerman MsgAuthorsubmissionIssueId = "Ausgabe"
renderMessageGerman MsgAuthorsubmissionHeadline = "Headline"
renderMessageGerman MsgAuthorsubmissionSubline = "Subline"
renderMessageGerman MsgAuthorsubmissionText = "Text"
renderMessageGerman MsgAuthorsubmissionfileSubmissionId = ""
renderMessageGerman MsgAuthorsubmissionfileRawdataId = ""
renderMessageGerman MsgAuthorsubmissionfileFilename = "Dateiname"
renderMessageGerman MsgAuthorsubmissionfileMimetype = "MIME Type"
renderMessageGerman MsgAuthorsubmissionfileSize = "Groesse"
renderMessageGerman MsgAuthorsubmissionfileFile = "Datei"
renderMessageGerman MsgIssueName = "Name"
renderMessageGerman MsgEditorsubmissionIssueId = "Ausgabe"
renderMessageGerman MsgEditorsubmissionRubricTypeId = "Rubrik"
renderMessageGerman MsgEditorsubmissionHeadline = "Headline"
renderMessageGerman MsgEditorsubmissionSubline = "Subline"
renderMessageGerman MsgEditorsubmissionText = "Text"
renderMessageGerman MsgEditorsubmissionfileSubmissionId = ""
renderMessageGerman MsgEditorsubmissionfileRawdataId = ""
renderMessageGerman MsgEditorsubmissionfileFilename = "Dateiname"
renderMessageGerman MsgEditorsubmissionfileMimetype = "MIME Type"
renderMessageGerman MsgEditorsubmissionfileSize = "Groesse"
renderMessageGerman MsgEditorsubmissionfileFile = "Datei"
renderMessageGerman MsgRubricTypeName = "Name"
renderMessageGerman MsgRubricTypeSortIndex = "Sortierungs-Index"
renderMessageGerman MsgUserUsers = "Nutzer"
renderMessageGerman MsgUserAddUser = "Nutzer hinzufügen"
renderMessageGerman MsgUserEditUser = "Nutzer bearbeiten"
renderMessageGerman MsgUserDeleteUser = "Nutzer löschen"
renderMessageGerman MsgConfigConfigurations = "Konfigurationen"
renderMessageGerman MsgConfigEditConfig = "Konfiguration bearbeiten"
renderMessageGerman MsgAuthorsubmissionSubmission = "Beitrag"
renderMessageGerman MsgAuthorsubmissionSubmissions = "Beiträge"
renderMessageGerman MsgAuthorsubmissionAddSubmission = "Beitrag hinzufügen"
renderMessageGerman MsgAuthorsubmissionEditSubmission = "Beitrag bearbeiten"
renderMessageGerman MsgAuthorsubmissionDeleteSubmission = "Beitrag löschen"
renderMessageGerman MsgAuthorsubmissionfileSubmissionfiles = "Dateien"
renderMessageGerman MsgAuthorsubmissionfileAddSubmissionfile = "Datei hinzufügen"
renderMessageGerman MsgAuthorsubmissionfileEditSubmissionfile = "Datei bearbeiten"
renderMessageGerman MsgAuthorsubmissionfileDeleteSubmissionfile = "Datei löschen"
renderMessageGerman MsgAuthorsubmissionfileDownloadSubmissionfile = "Datei runterladen"
renderMessageGerman MsgIssueIssue = "Ausgabe"
renderMessageGerman MsgIssueIssues = "Ausgaben"
renderMessageGerman MsgIssueAddIssue = "Ausgabe hinzufügen"
renderMessageGerman MsgIssueEditIssue = "Ausgabe bearbeiten"
renderMessageGerman MsgIssueDeleteIssue = "Ausgabe löschen"
renderMessageGerman MsgEditorsubmissionSubmission = "Beitrag"
renderMessageGerman MsgEditorsubmissionSubmissions = "Beiträge"
renderMessageGerman MsgEditorsubmissionAddSubmission = "Beitrag hinzufügen"
renderMessageGerman MsgEditorsubmissionEditSubmission = "Beitrag bearbeiten"
renderMessageGerman MsgEditorsubmissionDeleteSubmission = "Beitrag löschen"
renderMessageGerman MsgEditorsubmissionfileSubmissionfiles = "Dateien"
renderMessageGerman MsgEditorsubmissionfileAddSubmissionfile = "Datei hinzufügen"
renderMessageGerman MsgEditorsubmissionfileEditSubmissionfile = "Datei bearbeiten"
renderMessageGerman MsgEditorsubmissionfileDeleteSubmissionfile = "Datei löschen"
renderMessageGerman MsgEditorsubmissionfileDownloadSubmissionfile = "Datei runterladen"
renderMessageGerman MsgRubricTypeRubric = "Rubrik"
renderMessageGerman MsgRubricTypeRubricTypes = "Rubrik-Typen"
renderMessageGerman MsgRubricTypeAddRubricType = "Rubrik-Typ hinzufügen"
renderMessageGerman MsgRubricTypeDeleteRubricType = "Rubrik-Typ löschen"
renderMessageGerman MsgRubricTypeEditRubricType = "Rubrik-Typ bearbeiten"
renderMessageGerman MsgTestmailEmail = "Email"
renderMessageGerman MsgTestmailTestMail = "Test-Mail"
renderMessageGerman MsgTestmailSendTestMail = "Test-Mail senden..."

renderMessageEnglish :: AppMessage -> Text
renderMessageEnglish MsgGlobalHome = "Home"
renderMessageEnglish MsgGlobalAdmin = "Admin"
renderMessageEnglish MsgGlobalLogout = "Logout"
renderMessageEnglish MsgGlobalLanguage = "Language"
renderMessageEnglish MsgGlobalMyProfile = "My Profile"
renderMessageEnglish MsgGlobalEditMyProfile = "Edit my profile"
renderMessageEnglish MsgGlobalReallyDelete = "Are you sure to delete?"
renderMessageEnglish MsgGlobalCancel = "Cancel"
renderMessageEnglish MsgGlobalEditor = "Editor"
renderMessageEnglish MsgGlobalReviewer = "Reviewer"
renderMessageEnglish MsgGlobalAuthor = "Author"
renderMessageEnglish MsgUserIdent = "Login"
renderMessageEnglish MsgUserPassword = "Password"
renderMessageEnglish MsgUserEmail = "Email"
renderMessageEnglish MsgUserIsAdmin = "Is admin?"
renderMessageEnglish MsgUserIsEditor = "Is editor?"
renderMessageEnglish MsgUserIsReviewer = "Is reviewer?"
renderMessageEnglish MsgUserIsAuthor = "Is author?"
renderMessageEnglish MsgUserIsResetPassword = "Generate new password? (Will be sent by email)"
renderMessageEnglish MsgConfigCode = "Code"
renderMessageEnglish MsgConfigStringValue = "String-Value"
renderMessageEnglish MsgConfigIntValue = "Integer-Value"
renderMessageEnglish MsgConfigDoubleValue = "Double-Value"
renderMessageEnglish MsgConfigBoolValue = "Boolean-Value"
renderMessageEnglish MsgRawdataBytes = "Bytes"
renderMessageEnglish MsgSubmissionIssueId = ""
renderMessageEnglish MsgSubmissionRubricTypeId = ""
renderMessageEnglish MsgSubmissionHeadline = ""
renderMessageEnglish MsgSubmissionSubline = ""
renderMessageEnglish MsgSubmissionText = ""
renderMessageEnglish MsgSubmissionfileSubmissionId = ""
renderMessageEnglish MsgSubmissionfileRawdataId = ""
renderMessageEnglish MsgSubmissionfileFilename = "Filename"
renderMessageEnglish MsgSubmissionfileMimetype = "MIME Type"
renderMessageEnglish MsgSubmissionfileSize = "Size"
renderMessageEnglish MsgAuthorsubmissionIssueId = "Issue"
renderMessageEnglish MsgAuthorsubmissionHeadline = "Headline"
renderMessageEnglish MsgAuthorsubmissionSubline = "Subline"
renderMessageEnglish MsgAuthorsubmissionText = "Text"
renderMessageEnglish MsgAuthorsubmissionfileSubmissionId = ""
renderMessageEnglish MsgAuthorsubmissionfileRawdataId = ""
renderMessageEnglish MsgAuthorsubmissionfileFilename = "Filename"
renderMessageEnglish MsgAuthorsubmissionfileMimetype = "MIME Type"
renderMessageEnglish MsgAuthorsubmissionfileSize = "Size"
renderMessageEnglish MsgAuthorsubmissionfileFile = "File"
renderMessageEnglish MsgIssueName = "Name"
renderMessageEnglish MsgEditorsubmissionIssueId = "Issue"
renderMessageEnglish MsgEditorsubmissionRubricTypeId = "Rubric"
renderMessageEnglish MsgEditorsubmissionHeadline = "Headline"
renderMessageEnglish MsgEditorsubmissionSubline = "Subline"
renderMessageEnglish MsgEditorsubmissionText = "Text"
renderMessageEnglish MsgEditorsubmissionfileSubmissionId = ""
renderMessageEnglish MsgEditorsubmissionfileRawdataId = ""
renderMessageEnglish MsgEditorsubmissionfileFilename = "Filename"
renderMessageEnglish MsgEditorsubmissionfileMimetype = "MIME Type"
renderMessageEnglish MsgEditorsubmissionfileSize = "Size"
renderMessageEnglish MsgEditorsubmissionfileFile = "File"
renderMessageEnglish MsgRubricTypeName = "Name"
renderMessageEnglish MsgRubricTypeSortIndex = "Sort Index"
renderMessageEnglish MsgUserUsers = "Users"
renderMessageEnglish MsgUserAddUser = "Add user"
renderMessageEnglish MsgUserEditUser = "Edit user"
renderMessageEnglish MsgUserDeleteUser = "Delete user"
renderMessageEnglish MsgConfigConfigurations = "Configurations"
renderMessageEnglish MsgConfigEditConfig = "Edit config"
renderMessageEnglish MsgAuthorsubmissionSubmission = "Submission"
renderMessageEnglish MsgAuthorsubmissionSubmissions = "Submissions"
renderMessageEnglish MsgAuthorsubmissionAddSubmission = "Add submission"
renderMessageEnglish MsgAuthorsubmissionEditSubmission = "Edit submission"
renderMessageEnglish MsgAuthorsubmissionDeleteSubmission = "Delete submission"
renderMessageEnglish MsgAuthorsubmissionfileSubmissionfiles = "Files"
renderMessageEnglish MsgAuthorsubmissionfileAddSubmissionfile = "Add file"
renderMessageEnglish MsgAuthorsubmissionfileEditSubmissionfile = "Edit file"
renderMessageEnglish MsgAuthorsubmissionfileDeleteSubmissionfile = "Delete file"
renderMessageEnglish MsgAuthorsubmissionfileDownloadSubmissionfile = "Download file"
renderMessageEnglish MsgIssueIssue = "Issue"
renderMessageEnglish MsgIssueIssues = "Issues"
renderMessageEnglish MsgIssueAddIssue = "Add issue"
renderMessageEnglish MsgIssueEditIssue = "Edit issue"
renderMessageEnglish MsgIssueDeleteIssue = "Delete issue"
renderMessageEnglish MsgEditorsubmissionSubmission = "Submission"
renderMessageEnglish MsgEditorsubmissionSubmissions = "Submissions"
renderMessageEnglish MsgEditorsubmissionAddSubmission = "Add submission"
renderMessageEnglish MsgEditorsubmissionEditSubmission = "Edit submission"
renderMessageEnglish MsgEditorsubmissionDeleteSubmission = "Delete submission"
renderMessageEnglish MsgEditorsubmissionfileSubmissionfiles = "Files"
renderMessageEnglish MsgEditorsubmissionfileAddSubmissionfile = "Add file"
renderMessageEnglish MsgEditorsubmissionfileEditSubmissionfile = "Edit file"
renderMessageEnglish MsgEditorsubmissionfileDeleteSubmissionfile = "Delete file"
renderMessageEnglish MsgEditorsubmissionfileDownloadSubmissionfile = "Download file"
renderMessageEnglish MsgRubricTypeRubric = "Rubric"
renderMessageEnglish MsgRubricTypeRubricTypes = "Rubric types"
renderMessageEnglish MsgRubricTypeAddRubricType = "Add rubric type"
renderMessageEnglish MsgRubricTypeDeleteRubricType = "Delete rubric type"
renderMessageEnglish MsgRubricTypeEditRubricType = "Edit rubric type"
renderMessageEnglish MsgTestmailEmail = "Email"
renderMessageEnglish MsgTestmailTestMail = "Test-Mail"
renderMessageEnglish MsgTestmailSendTestMail = "Send Test-Mail..."

data Translation = Translation
  { msgGlobalHome :: Text
  , msgGlobalAdmin :: Text
  , msgGlobalLogout :: Text
  , msgGlobalLanguage :: Text
  , msgGlobalMyProfile :: Text
  , msgGlobalEditMyProfile :: Text
  , msgGlobalReallyDelete :: Text
  , msgGlobalCancel :: Text
  , msgGlobalEditor :: Text
  , msgGlobalReviewer :: Text
  , msgGlobalAuthor :: Text
  , msgUserIdent :: Text
  , msgUserPassword :: Text
  , msgUserEmail :: Text
  , msgUserIsAdmin :: Text
  , msgUserIsEditor :: Text
  , msgUserIsReviewer :: Text
  , msgUserIsAuthor :: Text
  , msgUserIsResetPassword :: Text
  , msgConfigCode :: Text
  , msgConfigStringValue :: Text
  , msgConfigIntValue :: Text
  , msgConfigDoubleValue :: Text
  , msgConfigBoolValue :: Text
  , msgRawdataBytes :: Text
  , msgSubmissionIssueId :: Text
  , msgSubmissionRubricTypeId :: Text
  , msgSubmissionHeadline :: Text
  , msgSubmissionSubline :: Text
  , msgSubmissionText :: Text
  , msgSubmissionfileSubmissionId :: Text
  , msgSubmissionfileRawdataId :: Text
  , msgSubmissionfileFilename :: Text
  , msgSubmissionfileMimetype :: Text
  , msgSubmissionfileSize :: Text
  , msgAuthorsubmissionIssueId :: Text
  , msgAuthorsubmissionHeadline :: Text
  , msgAuthorsubmissionSubline :: Text
  , msgAuthorsubmissionText :: Text
  , msgAuthorsubmissionfileSubmissionId :: Text
  , msgAuthorsubmissionfileRawdataId :: Text
  , msgAuthorsubmissionfileFilename :: Text
  , msgAuthorsubmissionfileMimetype :: Text
  , msgAuthorsubmissionfileSize :: Text
  , msgAuthorsubmissionfileFile :: Text
  , msgIssueName :: Text
  , msgEditorsubmissionIssueId :: Text
  , msgEditorsubmissionRubricTypeId :: Text
  , msgEditorsubmissionHeadline :: Text
  , msgEditorsubmissionSubline :: Text
  , msgEditorsubmissionText :: Text
  , msgEditorsubmissionfileSubmissionId :: Text
  , msgEditorsubmissionfileRawdataId :: Text
  , msgEditorsubmissionfileFilename :: Text
  , msgEditorsubmissionfileMimetype :: Text
  , msgEditorsubmissionfileSize :: Text
  , msgEditorsubmissionfileFile :: Text
  , msgRubricTypeName :: Text
  , msgRubricTypeSortIndex :: Text
  , msgUserUsers :: Text
  , msgUserAddUser :: Text
  , msgUserEditUser :: Text
  , msgUserDeleteUser :: Text
  , msgConfigConfigurations :: Text
  , msgConfigEditConfig :: Text
  , msgAuthorsubmissionSubmission :: Text
  , msgAuthorsubmissionSubmissions :: Text
  , msgAuthorsubmissionAddSubmission :: Text
  , msgAuthorsubmissionEditSubmission :: Text
  , msgAuthorsubmissionDeleteSubmission :: Text
  , msgAuthorsubmissionfileSubmissionfiles :: Text
  , msgAuthorsubmissionfileAddSubmissionfile :: Text
  , msgAuthorsubmissionfileEditSubmissionfile :: Text
  , msgAuthorsubmissionfileDeleteSubmissionfile :: Text
  , msgAuthorsubmissionfileDownloadSubmissionfile :: Text
  , msgIssueIssue :: Text
  , msgIssueIssues :: Text
  , msgIssueAddIssue :: Text
  , msgIssueEditIssue :: Text
  , msgIssueDeleteIssue :: Text
  , msgEditorsubmissionSubmission :: Text
  , msgEditorsubmissionSubmissions :: Text
  , msgEditorsubmissionAddSubmission :: Text
  , msgEditorsubmissionEditSubmission :: Text
  , msgEditorsubmissionDeleteSubmission :: Text
  , msgEditorsubmissionfileSubmissionfiles :: Text
  , msgEditorsubmissionfileAddSubmissionfile :: Text
  , msgEditorsubmissionfileEditSubmissionfile :: Text
  , msgEditorsubmissionfileDeleteSubmissionfile :: Text
  , msgEditorsubmissionfileDownloadSubmissionfile :: Text
  , msgRubricTypeRubric :: Text
  , msgRubricTypeRubricTypes :: Text
  , msgRubricTypeAddRubricType :: Text
  , msgRubricTypeDeleteRubricType :: Text
  , msgRubricTypeEditRubricType :: Text
  , msgTestmailEmail :: Text
  , msgTestmailTestMail :: Text
  , msgTestmailSendTestMail :: Text
  } deriving Generic

instance ToJSON Translation

translationDe :: Translation
translationDe = Translation
  { msgGlobalHome = "Home"
  , msgGlobalAdmin = "Admin"
  , msgGlobalLogout = "Logout"
  , msgGlobalLanguage = "Sprache"
  , msgGlobalMyProfile = "Mein Profil"
  , msgGlobalEditMyProfile = "Mein Profil bearbeiten"
  , msgGlobalReallyDelete = "Möchten sie wirklich löschen?"
  , msgGlobalCancel = "Abbrechen"
  , msgGlobalEditor = "Redakteur"
  , msgGlobalReviewer = "Reviewer"
  , msgGlobalAuthor = "Autor"
  , msgUserIdent = "Login"
  , msgUserPassword = "Passwort"
  , msgUserEmail = "Email"
  , msgUserIsAdmin = "Ist Admin?"
  , msgUserIsEditor = "Ist Redaktuer?"
  , msgUserIsReviewer = "Ist Gutachter?"
  , msgUserIsAuthor = "Ist Autor?"
  , msgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)"
  , msgConfigCode = "Code"
  , msgConfigStringValue = "String-Wert"
  , msgConfigIntValue = "Integer-Wert"
  , msgConfigDoubleValue = "Double-Wert"
  , msgConfigBoolValue = "Boolean-Wert"
  , msgRawdataBytes = "Bytes"
  , msgSubmissionIssueId = ""
  , msgSubmissionRubricTypeId = ""
  , msgSubmissionHeadline = ""
  , msgSubmissionSubline = ""
  , msgSubmissionText = ""
  , msgSubmissionfileSubmissionId = ""
  , msgSubmissionfileRawdataId = ""
  , msgSubmissionfileFilename = "Dateiname"
  , msgSubmissionfileMimetype = "MIME Type"
  , msgSubmissionfileSize = "Groesse"
  , msgAuthorsubmissionIssueId = "Ausgabe"
  , msgAuthorsubmissionHeadline = "Headline"
  , msgAuthorsubmissionSubline = "Subline"
  , msgAuthorsubmissionText = "Text"
  , msgAuthorsubmissionfileSubmissionId = ""
  , msgAuthorsubmissionfileRawdataId = ""
  , msgAuthorsubmissionfileFilename = "Dateiname"
  , msgAuthorsubmissionfileMimetype = "MIME Type"
  , msgAuthorsubmissionfileSize = "Groesse"
  , msgAuthorsubmissionfileFile = "Datei"
  , msgIssueName = "Name"
  , msgEditorsubmissionIssueId = "Ausgabe"
  , msgEditorsubmissionRubricTypeId = "Rubrik"
  , msgEditorsubmissionHeadline = "Headline"
  , msgEditorsubmissionSubline = "Subline"
  , msgEditorsubmissionText = "Text"
  , msgEditorsubmissionfileSubmissionId = ""
  , msgEditorsubmissionfileRawdataId = ""
  , msgEditorsubmissionfileFilename = "Dateiname"
  , msgEditorsubmissionfileMimetype = "MIME Type"
  , msgEditorsubmissionfileSize = "Groesse"
  , msgEditorsubmissionfileFile = "Datei"
  , msgRubricTypeName = "Name"
  , msgRubricTypeSortIndex = "Sortierungs-Index"
  , msgUserUsers = "Nutzer"
  , msgUserAddUser = "Nutzer hinzufügen"
  , msgUserEditUser = "Nutzer bearbeiten"
  , msgUserDeleteUser = "Nutzer löschen"
  , msgConfigConfigurations = "Konfigurationen"
  , msgConfigEditConfig = "Konfiguration bearbeiten"
  , msgAuthorsubmissionSubmission = "Beitrag"
  , msgAuthorsubmissionSubmissions = "Beiträge"
  , msgAuthorsubmissionAddSubmission = "Beitrag hinzufügen"
  , msgAuthorsubmissionEditSubmission = "Beitrag bearbeiten"
  , msgAuthorsubmissionDeleteSubmission = "Beitrag löschen"
  , msgAuthorsubmissionfileSubmissionfiles = "Dateien"
  , msgAuthorsubmissionfileAddSubmissionfile = "Datei hinzufügen"
  , msgAuthorsubmissionfileEditSubmissionfile = "Datei bearbeiten"
  , msgAuthorsubmissionfileDeleteSubmissionfile = "Datei löschen"
  , msgAuthorsubmissionfileDownloadSubmissionfile = "Datei runterladen"
  , msgIssueIssue = "Ausgabe"
  , msgIssueIssues = "Ausgaben"
  , msgIssueAddIssue = "Ausgabe hinzufügen"
  , msgIssueEditIssue = "Ausgabe bearbeiten"
  , msgIssueDeleteIssue = "Ausgabe löschen"
  , msgEditorsubmissionSubmission = "Beitrag"
  , msgEditorsubmissionSubmissions = "Beiträge"
  , msgEditorsubmissionAddSubmission = "Beitrag hinzufügen"
  , msgEditorsubmissionEditSubmission = "Beitrag bearbeiten"
  , msgEditorsubmissionDeleteSubmission = "Beitrag löschen"
  , msgEditorsubmissionfileSubmissionfiles = "Dateien"
  , msgEditorsubmissionfileAddSubmissionfile = "Datei hinzufügen"
  , msgEditorsubmissionfileEditSubmissionfile = "Datei bearbeiten"
  , msgEditorsubmissionfileDeleteSubmissionfile = "Datei löschen"
  , msgEditorsubmissionfileDownloadSubmissionfile = "Datei runterladen"
  , msgRubricTypeRubric = "Rubrik"
  , msgRubricTypeRubricTypes = "Rubrik-Typen"
  , msgRubricTypeAddRubricType = "Rubrik-Typ hinzufügen"
  , msgRubricTypeDeleteRubricType = "Rubrik-Typ löschen"
  , msgRubricTypeEditRubricType = "Rubrik-Typ bearbeiten"
  , msgTestmailEmail = "Email"
  , msgTestmailTestMail = "Test-Mail"
  , msgTestmailSendTestMail = "Test-Mail senden..."}

translationEn :: Translation
translationEn = Translation
  { msgGlobalHome = "Home"
  , msgGlobalAdmin = "Admin"
  , msgGlobalLogout = "Logout"
  , msgGlobalLanguage = "Language"
  , msgGlobalMyProfile = "My Profile"
  , msgGlobalEditMyProfile = "Edit my profile"
  , msgGlobalReallyDelete = "Are you sure to delete?"
  , msgGlobalCancel = "Cancel"
  , msgGlobalEditor = "Editor"
  , msgGlobalReviewer = "Reviewer"
  , msgGlobalAuthor = "Author"
  , msgUserIdent = "Login"
  , msgUserPassword = "Password"
  , msgUserEmail = "Email"
  , msgUserIsAdmin = "Is admin?"
  , msgUserIsEditor = "Is editor?"
  , msgUserIsReviewer = "Is reviewer?"
  , msgUserIsAuthor = "Is author?"
  , msgUserIsResetPassword = "Generate new password? (Will be sent by email)"
  , msgConfigCode = "Code"
  , msgConfigStringValue = "String-Value"
  , msgConfigIntValue = "Integer-Value"
  , msgConfigDoubleValue = "Double-Value"
  , msgConfigBoolValue = "Boolean-Value"
  , msgRawdataBytes = "Bytes"
  , msgSubmissionIssueId = ""
  , msgSubmissionRubricTypeId = ""
  , msgSubmissionHeadline = ""
  , msgSubmissionSubline = ""
  , msgSubmissionText = ""
  , msgSubmissionfileSubmissionId = ""
  , msgSubmissionfileRawdataId = ""
  , msgSubmissionfileFilename = "Filename"
  , msgSubmissionfileMimetype = "MIME Type"
  , msgSubmissionfileSize = "Size"
  , msgAuthorsubmissionIssueId = "Issue"
  , msgAuthorsubmissionHeadline = "Headline"
  , msgAuthorsubmissionSubline = "Subline"
  , msgAuthorsubmissionText = "Text"
  , msgAuthorsubmissionfileSubmissionId = ""
  , msgAuthorsubmissionfileRawdataId = ""
  , msgAuthorsubmissionfileFilename = "Filename"
  , msgAuthorsubmissionfileMimetype = "MIME Type"
  , msgAuthorsubmissionfileSize = "Size"
  , msgAuthorsubmissionfileFile = "File"
  , msgIssueName = "Name"
  , msgEditorsubmissionIssueId = "Issue"
  , msgEditorsubmissionRubricTypeId = "Rubric"
  , msgEditorsubmissionHeadline = "Headline"
  , msgEditorsubmissionSubline = "Subline"
  , msgEditorsubmissionText = "Text"
  , msgEditorsubmissionfileSubmissionId = ""
  , msgEditorsubmissionfileRawdataId = ""
  , msgEditorsubmissionfileFilename = "Filename"
  , msgEditorsubmissionfileMimetype = "MIME Type"
  , msgEditorsubmissionfileSize = "Size"
  , msgEditorsubmissionfileFile = "File"
  , msgRubricTypeName = "Name"
  , msgRubricTypeSortIndex = "Sort Index"
  , msgUserUsers = "Users"
  , msgUserAddUser = "Add user"
  , msgUserEditUser = "Edit user"
  , msgUserDeleteUser = "Delete user"
  , msgConfigConfigurations = "Configurations"
  , msgConfigEditConfig = "Edit config"
  , msgAuthorsubmissionSubmission = "Submission"
  , msgAuthorsubmissionSubmissions = "Submissions"
  , msgAuthorsubmissionAddSubmission = "Add submission"
  , msgAuthorsubmissionEditSubmission = "Edit submission"
  , msgAuthorsubmissionDeleteSubmission = "Delete submission"
  , msgAuthorsubmissionfileSubmissionfiles = "Files"
  , msgAuthorsubmissionfileAddSubmissionfile = "Add file"
  , msgAuthorsubmissionfileEditSubmissionfile = "Edit file"
  , msgAuthorsubmissionfileDeleteSubmissionfile = "Delete file"
  , msgAuthorsubmissionfileDownloadSubmissionfile = "Download file"
  , msgIssueIssue = "Issue"
  , msgIssueIssues = "Issues"
  , msgIssueAddIssue = "Add issue"
  , msgIssueEditIssue = "Edit issue"
  , msgIssueDeleteIssue = "Delete issue"
  , msgEditorsubmissionSubmission = "Submission"
  , msgEditorsubmissionSubmissions = "Submissions"
  , msgEditorsubmissionAddSubmission = "Add submission"
  , msgEditorsubmissionEditSubmission = "Edit submission"
  , msgEditorsubmissionDeleteSubmission = "Delete submission"
  , msgEditorsubmissionfileSubmissionfiles = "Files"
  , msgEditorsubmissionfileAddSubmissionfile = "Add file"
  , msgEditorsubmissionfileEditSubmissionfile = "Edit file"
  , msgEditorsubmissionfileDeleteSubmissionfile = "Delete file"
  , msgEditorsubmissionfileDownloadSubmissionfile = "Download file"
  , msgRubricTypeRubric = "Rubric"
  , msgRubricTypeRubricTypes = "Rubric types"
  , msgRubricTypeAddRubricType = "Add rubric type"
  , msgRubricTypeDeleteRubricType = "Delete rubric type"
  , msgRubricTypeEditRubricType = "Edit rubric type"
  , msgTestmailEmail = "Email"
  , msgTestmailTestMail = "Test-Mail"
  , msgTestmailSendTestMail = "Send Test-Mail..."}

-- gen i18n - end
