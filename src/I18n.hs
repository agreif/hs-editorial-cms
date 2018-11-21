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
  | MsgTestmailEmail
  | MsgRawdataBytes
  | MsgSubmissionHeadline
  | MsgSubmissionSubline
  | MsgSubmissionText
  | MsgSubmissionfileSubmissionId
  | MsgSubmissionfileRawdataId
  | MsgSubmissionfileFilename
  | MsgSubmissionfileMimetype
  | MsgSubmissionfileSize
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
  | MsgEditorsubmissionHeadline
  | MsgEditorsubmissionSubline
  | MsgEditorsubmissionText
  | MsgEditorsubmissionfileSubmissionId
  | MsgEditorsubmissionfileRawdataId
  | MsgEditorsubmissionfileFilename
  | MsgEditorsubmissionfileMimetype
  | MsgEditorsubmissionfileSize
  | MsgEditorsubmissionfileFile
  | MsgUserUsers
  | MsgUserAddUser
  | MsgUserEditUser
  | MsgUserDeleteUser
  | MsgConfigConfigurations
  | MsgConfigEditConfig
  | MsgTestmailTestMail
  | MsgTestmailSendTestMail
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
renderMessageGerman MsgTestmailEmail = "Email"
renderMessageGerman MsgRawdataBytes = "Bytes"
renderMessageGerman MsgSubmissionHeadline = "Headline"
renderMessageGerman MsgSubmissionSubline = "Subline"
renderMessageGerman MsgSubmissionText = "Text"
renderMessageGerman MsgSubmissionfileSubmissionId = "Kunde"
renderMessageGerman MsgSubmissionfileRawdataId = ""
renderMessageGerman MsgSubmissionfileFilename = "Dateiname"
renderMessageGerman MsgSubmissionfileMimetype = "MIME Type"
renderMessageGerman MsgSubmissionfileSize = "Groesse"
renderMessageGerman MsgAuthorsubmissionHeadline = "Headline"
renderMessageGerman MsgAuthorsubmissionSubline = "Subline"
renderMessageGerman MsgAuthorsubmissionText = "Text"
renderMessageGerman MsgAuthorsubmissionfileSubmissionId = "Kunde"
renderMessageGerman MsgAuthorsubmissionfileRawdataId = ""
renderMessageGerman MsgAuthorsubmissionfileFilename = "Dateiname"
renderMessageGerman MsgAuthorsubmissionfileMimetype = "MIME Type"
renderMessageGerman MsgAuthorsubmissionfileSize = "Groesse"
renderMessageGerman MsgAuthorsubmissionfileFile = "Datei"
renderMessageGerman MsgIssueName = "Name"
renderMessageGerman MsgEditorsubmissionHeadline = "Headline"
renderMessageGerman MsgEditorsubmissionSubline = "Subline"
renderMessageGerman MsgEditorsubmissionText = "Text"
renderMessageGerman MsgEditorsubmissionfileSubmissionId = "Kunde"
renderMessageGerman MsgEditorsubmissionfileRawdataId = ""
renderMessageGerman MsgEditorsubmissionfileFilename = "Dateiname"
renderMessageGerman MsgEditorsubmissionfileMimetype = "MIME Type"
renderMessageGerman MsgEditorsubmissionfileSize = "Groesse"
renderMessageGerman MsgEditorsubmissionfileFile = "Datei"
renderMessageGerman MsgUserUsers = "Nutzer"
renderMessageGerman MsgUserAddUser = "Nutzer hinzufügen"
renderMessageGerman MsgUserEditUser = "Nutzer bearbeiten"
renderMessageGerman MsgUserDeleteUser = "Nutzer löschen"
renderMessageGerman MsgConfigConfigurations = "Konfigurationen"
renderMessageGerman MsgConfigEditConfig = "Konfiguration bearbeiten"
renderMessageGerman MsgTestmailTestMail = "Test-Mail"
renderMessageGerman MsgTestmailSendTestMail = "Test-Mail senden..."
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
renderMessageEnglish MsgTestmailEmail = "Email"
renderMessageEnglish MsgRawdataBytes = "Bytes"
renderMessageEnglish MsgSubmissionHeadline = "Headline"
renderMessageEnglish MsgSubmissionSubline = "Subline"
renderMessageEnglish MsgSubmissionText = "Text"
renderMessageEnglish MsgSubmissionfileSubmissionId = "Submission"
renderMessageEnglish MsgSubmissionfileRawdataId = ""
renderMessageEnglish MsgSubmissionfileFilename = "Filename"
renderMessageEnglish MsgSubmissionfileMimetype = "MIME Type"
renderMessageEnglish MsgSubmissionfileSize = "Size"
renderMessageEnglish MsgAuthorsubmissionHeadline = "Headline"
renderMessageEnglish MsgAuthorsubmissionSubline = "Subline"
renderMessageEnglish MsgAuthorsubmissionText = "Text"
renderMessageEnglish MsgAuthorsubmissionfileSubmissionId = "Submission"
renderMessageEnglish MsgAuthorsubmissionfileRawdataId = ""
renderMessageEnglish MsgAuthorsubmissionfileFilename = "Filename"
renderMessageEnglish MsgAuthorsubmissionfileMimetype = "MIME Type"
renderMessageEnglish MsgAuthorsubmissionfileSize = "Size"
renderMessageEnglish MsgAuthorsubmissionfileFile = "File"
renderMessageEnglish MsgIssueName = "Name"
renderMessageEnglish MsgEditorsubmissionHeadline = "Headline"
renderMessageEnglish MsgEditorsubmissionSubline = "Subline"
renderMessageEnglish MsgEditorsubmissionText = "Text"
renderMessageEnglish MsgEditorsubmissionfileSubmissionId = "Submission"
renderMessageEnglish MsgEditorsubmissionfileRawdataId = ""
renderMessageEnglish MsgEditorsubmissionfileFilename = "Filename"
renderMessageEnglish MsgEditorsubmissionfileMimetype = "MIME Type"
renderMessageEnglish MsgEditorsubmissionfileSize = "Size"
renderMessageEnglish MsgEditorsubmissionfileFile = "File"
renderMessageEnglish MsgUserUsers = "Users"
renderMessageEnglish MsgUserAddUser = "Add user"
renderMessageEnglish MsgUserEditUser = "Edit user"
renderMessageEnglish MsgUserDeleteUser = "Delete user"
renderMessageEnglish MsgConfigConfigurations = "Configurations"
renderMessageEnglish MsgConfigEditConfig = "Edit config"
renderMessageEnglish MsgTestmailTestMail = "Test-Mail"
renderMessageEnglish MsgTestmailSendTestMail = "Send Test-Mail..."
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
  , msgTestmailEmail :: Text
  , msgRawdataBytes :: Text
  , msgSubmissionHeadline :: Text
  , msgSubmissionSubline :: Text
  , msgSubmissionText :: Text
  , msgSubmissionfileSubmissionId :: Text
  , msgSubmissionfileRawdataId :: Text
  , msgSubmissionfileFilename :: Text
  , msgSubmissionfileMimetype :: Text
  , msgSubmissionfileSize :: Text
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
  , msgEditorsubmissionHeadline :: Text
  , msgEditorsubmissionSubline :: Text
  , msgEditorsubmissionText :: Text
  , msgEditorsubmissionfileSubmissionId :: Text
  , msgEditorsubmissionfileRawdataId :: Text
  , msgEditorsubmissionfileFilename :: Text
  , msgEditorsubmissionfileMimetype :: Text
  , msgEditorsubmissionfileSize :: Text
  , msgEditorsubmissionfileFile :: Text
  , msgUserUsers :: Text
  , msgUserAddUser :: Text
  , msgUserEditUser :: Text
  , msgUserDeleteUser :: Text
  , msgConfigConfigurations :: Text
  , msgConfigEditConfig :: Text
  , msgTestmailTestMail :: Text
  , msgTestmailSendTestMail :: Text
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
  , msgTestmailEmail = "Email"
  , msgRawdataBytes = "Bytes"
  , msgSubmissionHeadline = "Headline"
  , msgSubmissionSubline = "Subline"
  , msgSubmissionText = "Text"
  , msgSubmissionfileSubmissionId = "Kunde"
  , msgSubmissionfileRawdataId = ""
  , msgSubmissionfileFilename = "Dateiname"
  , msgSubmissionfileMimetype = "MIME Type"
  , msgSubmissionfileSize = "Groesse"
  , msgAuthorsubmissionHeadline = "Headline"
  , msgAuthorsubmissionSubline = "Subline"
  , msgAuthorsubmissionText = "Text"
  , msgAuthorsubmissionfileSubmissionId = "Kunde"
  , msgAuthorsubmissionfileRawdataId = ""
  , msgAuthorsubmissionfileFilename = "Dateiname"
  , msgAuthorsubmissionfileMimetype = "MIME Type"
  , msgAuthorsubmissionfileSize = "Groesse"
  , msgAuthorsubmissionfileFile = "Datei"
  , msgIssueName = "Name"
  , msgEditorsubmissionHeadline = "Headline"
  , msgEditorsubmissionSubline = "Subline"
  , msgEditorsubmissionText = "Text"
  , msgEditorsubmissionfileSubmissionId = "Kunde"
  , msgEditorsubmissionfileRawdataId = ""
  , msgEditorsubmissionfileFilename = "Dateiname"
  , msgEditorsubmissionfileMimetype = "MIME Type"
  , msgEditorsubmissionfileSize = "Groesse"
  , msgEditorsubmissionfileFile = "Datei"
  , msgUserUsers = "Nutzer"
  , msgUserAddUser = "Nutzer hinzufügen"
  , msgUserEditUser = "Nutzer bearbeiten"
  , msgUserDeleteUser = "Nutzer löschen"
  , msgConfigConfigurations = "Konfigurationen"
  , msgConfigEditConfig = "Konfiguration bearbeiten"
  , msgTestmailTestMail = "Test-Mail"
  , msgTestmailSendTestMail = "Test-Mail senden..."
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
  , msgEditorsubmissionfileDownloadSubmissionfile = "Datei runterladen"}

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
  , msgTestmailEmail = "Email"
  , msgRawdataBytes = "Bytes"
  , msgSubmissionHeadline = "Headline"
  , msgSubmissionSubline = "Subline"
  , msgSubmissionText = "Text"
  , msgSubmissionfileSubmissionId = "Submission"
  , msgSubmissionfileRawdataId = ""
  , msgSubmissionfileFilename = "Filename"
  , msgSubmissionfileMimetype = "MIME Type"
  , msgSubmissionfileSize = "Size"
  , msgAuthorsubmissionHeadline = "Headline"
  , msgAuthorsubmissionSubline = "Subline"
  , msgAuthorsubmissionText = "Text"
  , msgAuthorsubmissionfileSubmissionId = "Submission"
  , msgAuthorsubmissionfileRawdataId = ""
  , msgAuthorsubmissionfileFilename = "Filename"
  , msgAuthorsubmissionfileMimetype = "MIME Type"
  , msgAuthorsubmissionfileSize = "Size"
  , msgAuthorsubmissionfileFile = "File"
  , msgIssueName = "Name"
  , msgEditorsubmissionHeadline = "Headline"
  , msgEditorsubmissionSubline = "Subline"
  , msgEditorsubmissionText = "Text"
  , msgEditorsubmissionfileSubmissionId = "Submission"
  , msgEditorsubmissionfileRawdataId = ""
  , msgEditorsubmissionfileFilename = "Filename"
  , msgEditorsubmissionfileMimetype = "MIME Type"
  , msgEditorsubmissionfileSize = "Size"
  , msgEditorsubmissionfileFile = "File"
  , msgUserUsers = "Users"
  , msgUserAddUser = "Add user"
  , msgUserEditUser = "Edit user"
  , msgUserDeleteUser = "Delete user"
  , msgConfigConfigurations = "Configurations"
  , msgConfigEditConfig = "Edit config"
  , msgTestmailTestMail = "Test-Mail"
  , msgTestmailSendTestMail = "Send Test-Mail..."
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
  , msgEditorsubmissionfileDownloadSubmissionfile = "Download file"}

-- gen i18n - end
