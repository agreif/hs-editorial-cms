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
  | MsgGlobalUsers
  | MsgGlobalAddUser
  | MsgGlobalEditUser
  | MsgGlobalDeleteUser
  | MsgGlobalConfigurations
  | MsgGlobalEditConfig
  | MsgGlobalTestMail
  | MsgGlobalSendTestMail
  | MsgGlobalCancel
  | MsgUserIdent
  | MsgUserPassword
  | MsgUserEmail
  | MsgUserIsAdmin
  | MsgUserIsResetPassword
  | MsgConfigCode
  | MsgConfigStringValue
  | MsgConfigIntValue
  | MsgConfigDoubleValue
  | MsgConfigBoolValue
  | MsgTestmailEmail

renderMessageGerman :: AppMessage -> Text
renderMessageGerman MsgGlobalHome = "Home"
renderMessageGerman MsgGlobalAdmin = "Admin"
renderMessageGerman MsgGlobalLogout = "Logout"
renderMessageGerman MsgGlobalLanguage = "Sprache"
renderMessageGerman MsgGlobalMyProfile = "Mein Profil"
renderMessageGerman MsgGlobalEditMyProfile = "Mein Profil bearbeiten"
renderMessageGerman MsgGlobalReallyDelete = "Möchten sie wirklich löschen?"
renderMessageGerman MsgGlobalUsers = "Nutzer"
renderMessageGerman MsgGlobalAddUser = "Nutzer hinzufügen"
renderMessageGerman MsgGlobalEditUser = "Nutzer bearbeiten"
renderMessageGerman MsgGlobalDeleteUser = "Nutzer löschen"
renderMessageGerman MsgGlobalConfigurations = "Konfigurationen"
renderMessageGerman MsgGlobalEditConfig = "Konfiguration bearbeiten"
renderMessageGerman MsgGlobalTestMail = "Test-Mail"
renderMessageGerman MsgGlobalSendTestMail = "Test-Mail senden..."
renderMessageGerman MsgGlobalCancel = "Abbrechen"
renderMessageGerman MsgUserIdent = "Login"
renderMessageGerman MsgUserPassword = "Passwort"
renderMessageGerman MsgUserEmail = "Email"
renderMessageGerman MsgUserIsAdmin = "Ist Admin?"
renderMessageGerman MsgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)"
renderMessageGerman MsgConfigCode = "Code"
renderMessageGerman MsgConfigStringValue = "String-Wert"
renderMessageGerman MsgConfigIntValue = "Integer-Wert"
renderMessageGerman MsgConfigDoubleValue = "Double-Wert"
renderMessageGerman MsgConfigBoolValue = "Boolean-Wert"
renderMessageGerman MsgTestmailEmail = "Email"

renderMessageEnglish :: AppMessage -> Text
renderMessageEnglish MsgGlobalHome = "Home"
renderMessageEnglish MsgGlobalAdmin = "Admin"
renderMessageEnglish MsgGlobalLogout = "Logout"
renderMessageEnglish MsgGlobalLanguage = "Language"
renderMessageEnglish MsgGlobalMyProfile = "My Profile"
renderMessageEnglish MsgGlobalEditMyProfile = "Edit my profile"
renderMessageEnglish MsgGlobalReallyDelete = "Are you sure to delete?"
renderMessageEnglish MsgGlobalUsers = "Users"
renderMessageEnglish MsgGlobalAddUser = "Add user"
renderMessageEnglish MsgGlobalEditUser = "Edit user"
renderMessageEnglish MsgGlobalDeleteUser = "Delete user"
renderMessageEnglish MsgGlobalConfigurations = "Configurations"
renderMessageEnglish MsgGlobalEditConfig = "Edit config"
renderMessageEnglish MsgGlobalTestMail = "Test-Mail"
renderMessageEnglish MsgGlobalSendTestMail = "Send Test-Mail..."
renderMessageEnglish MsgGlobalCancel = "Cancel"
renderMessageEnglish MsgUserIdent = "Login"
renderMessageEnglish MsgUserPassword = "Password"
renderMessageEnglish MsgUserEmail = "Email"
renderMessageEnglish MsgUserIsAdmin = "Is admin?"
renderMessageEnglish MsgUserIsResetPassword = "Generate new password? (Will be sent by email)"
renderMessageEnglish MsgConfigCode = "Code"
renderMessageEnglish MsgConfigStringValue = "String-Value"
renderMessageEnglish MsgConfigIntValue = "Integer-Value"
renderMessageEnglish MsgConfigDoubleValue = "Double-Value"
renderMessageEnglish MsgConfigBoolValue = "Boolean-Value"
renderMessageEnglish MsgTestmailEmail = "Email"

data Translation = Translation
  { msgGlobalHome :: Text
  , msgGlobalAdmin :: Text
  , msgGlobalLogout :: Text
  , msgGlobalLanguage :: Text
  , msgGlobalMyProfile :: Text
  , msgGlobalEditMyProfile :: Text
  , msgGlobalReallyDelete :: Text
  , msgGlobalUsers :: Text
  , msgGlobalAddUser :: Text
  , msgGlobalEditUser :: Text
  , msgGlobalDeleteUser :: Text
  , msgGlobalConfigurations :: Text
  , msgGlobalEditConfig :: Text
  , msgGlobalTestMail :: Text
  , msgGlobalSendTestMail :: Text
  , msgGlobalCancel :: Text
  , msgUserIdent :: Text
  , msgUserPassword :: Text
  , msgUserEmail :: Text
  , msgUserIsAdmin :: Text
  , msgUserIsResetPassword :: Text
  , msgConfigCode :: Text
  , msgConfigStringValue :: Text
  , msgConfigIntValue :: Text
  , msgConfigDoubleValue :: Text
  , msgConfigBoolValue :: Text
  , msgTestmailEmail :: Text
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
  , msgGlobalUsers = "Nutzer"
  , msgGlobalAddUser = "Nutzer hinzufügen"
  , msgGlobalEditUser = "Nutzer bearbeiten"
  , msgGlobalDeleteUser = "Nutzer löschen"
  , msgGlobalConfigurations = "Konfigurationen"
  , msgGlobalEditConfig = "Konfiguration bearbeiten"
  , msgGlobalTestMail = "Test-Mail"
  , msgGlobalSendTestMail = "Test-Mail senden..."
  , msgGlobalCancel = "Abbrechen"
  , msgUserIdent = "Login"
  , msgUserPassword = "Passwort"
  , msgUserEmail = "Email"
  , msgUserIsAdmin = "Ist Admin?"
  , msgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)"
  , msgConfigCode = "Code"
  , msgConfigStringValue = "String-Wert"
  , msgConfigIntValue = "Integer-Wert"
  , msgConfigDoubleValue = "Double-Wert"
  , msgConfigBoolValue = "Boolean-Wert"
  , msgTestmailEmail = "Email"}

translationEn :: Translation
translationEn = Translation
  { msgGlobalHome = "Home"
  , msgGlobalAdmin = "Admin"
  , msgGlobalLogout = "Logout"
  , msgGlobalLanguage = "Language"
  , msgGlobalMyProfile = "My Profile"
  , msgGlobalEditMyProfile = "Edit my profile"
  , msgGlobalReallyDelete = "Are you sure to delete?"
  , msgGlobalUsers = "Users"
  , msgGlobalAddUser = "Add user"
  , msgGlobalEditUser = "Edit user"
  , msgGlobalDeleteUser = "Delete user"
  , msgGlobalConfigurations = "Configurations"
  , msgGlobalEditConfig = "Edit config"
  , msgGlobalTestMail = "Test-Mail"
  , msgGlobalSendTestMail = "Send Test-Mail..."
  , msgGlobalCancel = "Cancel"
  , msgUserIdent = "Login"
  , msgUserPassword = "Password"
  , msgUserEmail = "Email"
  , msgUserIsAdmin = "Is admin?"
  , msgUserIsResetPassword = "Generate new password? (Will be sent by email)"
  , msgConfigCode = "Code"
  , msgConfigStringValue = "String-Value"
  , msgConfigIntValue = "Integer-Value"
  , msgConfigDoubleValue = "Double-Value"
  , msgConfigBoolValue = "Boolean-Value"
  , msgTestmailEmail = "Email"}

-- gen i18n - end
