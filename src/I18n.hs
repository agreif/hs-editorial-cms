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
  | MsgSubmissionHeadline
  | MsgSubmissionSubline
  | MsgUserUsers
  | MsgUserAddUser
  | MsgUserEditUser
  | MsgUserDeleteUser
  | MsgConfigConfigurations
  | MsgConfigEditConfig
  | MsgTestmailTestMail
  | MsgTestmailSendTestMail
  | MsgSubmissionSubmission
  | MsgSubmissionSubmissions
  | MsgSubmissionAddSubmission
  | MsgSubmissionEditSubmission
  | MsgSubmissionDeleteSubmission

renderMessageGerman :: AppMessage -> Text
renderMessageGerman MsgGlobalHome = "Home"
renderMessageGerman MsgGlobalAdmin = "Admin"
renderMessageGerman MsgGlobalLogout = "Logout"
renderMessageGerman MsgGlobalLanguage = "Sprache"
renderMessageGerman MsgGlobalMyProfile = "Mein Profil"
renderMessageGerman MsgGlobalEditMyProfile = "Mein Profil bearbeiten"
renderMessageGerman MsgGlobalReallyDelete = "Möchten sie wirklich löschen?"
renderMessageGerman MsgGlobalCancel = "Abbrechen"
renderMessageGerman MsgUserIdent = "Login"
renderMessageGerman MsgUserPassword = "Passwort"
renderMessageGerman MsgUserEmail = "Email"
renderMessageGerman MsgUserIsAdmin = "Ist Admin?"
renderMessageGerman MsgUserIsEditor = "Ist Redaktuer?"
renderMessageGerman MsgUserIsReviewer = "Ist Reviewer?"
renderMessageGerman MsgUserIsAuthor = "Ist Autor?"
renderMessageGerman MsgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)"
renderMessageGerman MsgConfigCode = "Code"
renderMessageGerman MsgConfigStringValue = "String-Wert"
renderMessageGerman MsgConfigIntValue = "Integer-Wert"
renderMessageGerman MsgConfigDoubleValue = "Double-Wert"
renderMessageGerman MsgConfigBoolValue = "Boolean-Wert"
renderMessageGerman MsgTestmailEmail = "Email"
renderMessageGerman MsgSubmissionHeadline = "Headline"
renderMessageGerman MsgSubmissionSubline = "Subline"
renderMessageGerman MsgUserUsers = "Nutzer"
renderMessageGerman MsgUserAddUser = "Nutzer hinzufügen"
renderMessageGerman MsgUserEditUser = "Nutzer bearbeiten"
renderMessageGerman MsgUserDeleteUser = "Nutzer löschen"
renderMessageGerman MsgConfigConfigurations = "Konfigurationen"
renderMessageGerman MsgConfigEditConfig = "Konfiguration bearbeiten"
renderMessageGerman MsgTestmailTestMail = "Test-Mail"
renderMessageGerman MsgTestmailSendTestMail = "Test-Mail senden..."
renderMessageGerman MsgSubmissionSubmission = "Beitrag"
renderMessageGerman MsgSubmissionSubmissions = "Beitraege"
renderMessageGerman MsgSubmissionAddSubmission = "Beitrag hinzufügen"
renderMessageGerman MsgSubmissionEditSubmission = "Beitrag bearbeiten"
renderMessageGerman MsgSubmissionDeleteSubmission = "Beitrag löschen"

renderMessageEnglish :: AppMessage -> Text
renderMessageEnglish MsgGlobalHome = "Home"
renderMessageEnglish MsgGlobalAdmin = "Admin"
renderMessageEnglish MsgGlobalLogout = "Logout"
renderMessageEnglish MsgGlobalLanguage = "Language"
renderMessageEnglish MsgGlobalMyProfile = "My Profile"
renderMessageEnglish MsgGlobalEditMyProfile = "Edit my profile"
renderMessageEnglish MsgGlobalReallyDelete = "Are you sure to delete?"
renderMessageEnglish MsgGlobalCancel = "Cancel"
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
renderMessageEnglish MsgSubmissionHeadline = "Headline"
renderMessageEnglish MsgSubmissionSubline = "Subline"
renderMessageEnglish MsgUserUsers = "Users"
renderMessageEnglish MsgUserAddUser = "Add user"
renderMessageEnglish MsgUserEditUser = "Edit user"
renderMessageEnglish MsgUserDeleteUser = "Delete user"
renderMessageEnglish MsgConfigConfigurations = "Configurations"
renderMessageEnglish MsgConfigEditConfig = "Edit config"
renderMessageEnglish MsgTestmailTestMail = "Test-Mail"
renderMessageEnglish MsgTestmailSendTestMail = "Send Test-Mail..."
renderMessageEnglish MsgSubmissionSubmission = "Submission"
renderMessageEnglish MsgSubmissionSubmissions = "Submissions"
renderMessageEnglish MsgSubmissionAddSubmission = "Add submission"
renderMessageEnglish MsgSubmissionEditSubmission = "Edit submission"
renderMessageEnglish MsgSubmissionDeleteSubmission = "Delete submission"

data Translation = Translation
  { msgGlobalHome :: Text
  , msgGlobalAdmin :: Text
  , msgGlobalLogout :: Text
  , msgGlobalLanguage :: Text
  , msgGlobalMyProfile :: Text
  , msgGlobalEditMyProfile :: Text
  , msgGlobalReallyDelete :: Text
  , msgGlobalCancel :: Text
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
  , msgSubmissionHeadline :: Text
  , msgSubmissionSubline :: Text
  , msgUserUsers :: Text
  , msgUserAddUser :: Text
  , msgUserEditUser :: Text
  , msgUserDeleteUser :: Text
  , msgConfigConfigurations :: Text
  , msgConfigEditConfig :: Text
  , msgTestmailTestMail :: Text
  , msgTestmailSendTestMail :: Text
  , msgSubmissionSubmission :: Text
  , msgSubmissionSubmissions :: Text
  , msgSubmissionAddSubmission :: Text
  , msgSubmissionEditSubmission :: Text
  , msgSubmissionDeleteSubmission :: Text
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
  , msgUserIdent = "Login"
  , msgUserPassword = "Passwort"
  , msgUserEmail = "Email"
  , msgUserIsAdmin = "Ist Admin?"
  , msgUserIsEditor = "Ist Redaktuer?"
  , msgUserIsReviewer = "Ist Reviewer?"
  , msgUserIsAuthor = "Ist Autor?"
  , msgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)"
  , msgConfigCode = "Code"
  , msgConfigStringValue = "String-Wert"
  , msgConfigIntValue = "Integer-Wert"
  , msgConfigDoubleValue = "Double-Wert"
  , msgConfigBoolValue = "Boolean-Wert"
  , msgTestmailEmail = "Email"
  , msgSubmissionHeadline = "Headline"
  , msgSubmissionSubline = "Subline"
  , msgUserUsers = "Nutzer"
  , msgUserAddUser = "Nutzer hinzufügen"
  , msgUserEditUser = "Nutzer bearbeiten"
  , msgUserDeleteUser = "Nutzer löschen"
  , msgConfigConfigurations = "Konfigurationen"
  , msgConfigEditConfig = "Konfiguration bearbeiten"
  , msgTestmailTestMail = "Test-Mail"
  , msgTestmailSendTestMail = "Test-Mail senden..."
  , msgSubmissionSubmission = "Beitrag"
  , msgSubmissionSubmissions = "Beitraege"
  , msgSubmissionAddSubmission = "Beitrag hinzufügen"
  , msgSubmissionEditSubmission = "Beitrag bearbeiten"
  , msgSubmissionDeleteSubmission = "Beitrag löschen"}

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
  , msgSubmissionHeadline = "Headline"
  , msgSubmissionSubline = "Subline"
  , msgUserUsers = "Users"
  , msgUserAddUser = "Add user"
  , msgUserEditUser = "Edit user"
  , msgUserDeleteUser = "Delete user"
  , msgConfigConfigurations = "Configurations"
  , msgConfigEditConfig = "Edit config"
  , msgTestmailTestMail = "Test-Mail"
  , msgTestmailSendTestMail = "Send Test-Mail..."
  , msgSubmissionSubmission = "Submission"
  , msgSubmissionSubmissions = "Submissions"
  , msgSubmissionAddSubmission = "Add submission"
  , msgSubmissionEditSubmission = "Edit submission"
  , msgSubmissionDeleteSubmission = "Delete submission"}

-- gen i18n - end
