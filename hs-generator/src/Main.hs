{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson

import Generator

main :: IO ()
main = generate context

-- model context

context :: Value
context =
  toJSON $
  BContext
  { bContextCrudModels =
      [ BCrudModel
        { bCrudModelName = "user"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = ["UniqueUser ident"]
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Nothing
        , bCrudModelDeleteFormDataJsonUrl = Just "AdminR AdminDataR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgUserAddUser"
        , bCrudModelEditFormTitleMsg = Just "MsgUserEditUser"
        , bCrudModelDeleteFormTitleMsg = Just "MsgUserDeleteUser"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "ident"
              , bCrudFieldLabelDe = Just "Login"
              , bCrudFieldLabelEn = Just "Login"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "password"
              , bCrudFieldLabelDe = Just "Passwort"
              , bCrudFieldLabelEn = Just "Password"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "email"
              , bCrudFieldLabelDe = Just "Email"
              , bCrudFieldLabelEn = Just "Email"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "isAdmin"
              , bCrudFieldLabelDe = Just "Ist Admin?"
              , bCrudFieldLabelEn = Just "Is admin?"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "isEditor"
              , bCrudFieldLabelDe = Just "Ist Redaktuer?"
              , bCrudFieldLabelEn = Just "Is editor?"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "isReviewer"
              , bCrudFieldLabelDe = Just "Ist Gutachter?"
              , bCrudFieldLabelEn = Just "Is reviewer?"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "isAuthor"
              , bCrudFieldLabelDe = Just "Ist Autor?"
              , bCrudFieldLabelEn = Just "Is author?"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "isResetPassword"
              , bCrudFieldLabelDe = Just "Neues Passwort generieren? (Wird per Email zugesendet)"
              , bCrudFieldLabelEn = Just "Generate new password? (Will be sent by email)"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Just "Nothing"
                  }
              }
            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "users", bTranslationDe = "Nutzer", bTranslationEn = "Users" }
          , BTranslation { bTranslationKey = "addUser", bTranslationDe = "Nutzer hinzufügen", bTranslationEn = "Add user" }
          , BTranslation { bTranslationKey = "editUser", bTranslationDe = "Nutzer bearbeiten", bTranslationEn = "Edit user" }
          , BTranslation { bTranslationKey = "deleteUser", bTranslationDe = "Nutzer löschen", bTranslationEn = "Delete user" }
          ]
        }
      , BCrudModel
        { bCrudModelName = "config"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = ["UniqueCode code"]
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Just "AdminR AdminDataR"
        , bCrudModelDeleteFormDataJsonUrl = Nothing
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Nothing
        , bCrudModelEditFormTitleMsg = Just "MsgConfigEditConfig"
        , bCrudModelDeleteFormTitleMsg = Nothing
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "code"
              , bCrudFieldLabelDe = Just "Code"
              , bCrudFieldLabelEn = Just "Code"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = True
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "stringValue"
              , bCrudFieldLabelDe = Just "String-Wert"
              , bCrudFieldLabelEn = Just "String-Value"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "intValue"
              , bCrudFieldLabelDe = Just "Integer-Wert"
              , bCrudFieldLabelEn = Just "Integer-Value"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "intField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "doubleValue"
              , bCrudFieldLabelDe = Just "Double-Wert"
              , bCrudFieldLabelEn = Just "Double-Value"
              , bCrudFieldHsType = "Double"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "doubleField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "boolValue"
              , bCrudFieldLabelDe = Just "Boolean-Wert"
              , bCrudFieldLabelEn = Just "Boolean-Value"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "configurations", bTranslationDe = "Konfigurationen", bTranslationEn = "Configurations" }
          , BTranslation { bTranslationKey = "editConfig", bTranslationDe = "Konfiguration bearbeiten", bTranslationEn = "Edit config" }
          ]
        }
      , BCrudModel
        { bCrudModelName = "testmail"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = False
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Just "EcmsR TestMailDataJsonR"
        , bCrudModelDeleteFormDataJsonUrl = Nothing
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgTestmailSendTestMail"
        , bCrudModelEditFormTitleMsg = Nothing
        , bCrudModelDeleteFormTitleMsg = Nothing
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "email"
              , bCrudFieldLabelDe = Just "Email"
              , bCrudFieldLabelEn = Just "Email"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView = Nothing
              }
            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "testMail", bTranslationDe = "Test-Mail", bTranslationEn = "Test-Mail" }
          , BTranslation { bTranslationKey = "sendTestMail", bTranslationDe = "Test-Mail senden...", bTranslationEn = "Send Test-Mail..." }
          ]
        }




      , BCrudModel
        { bCrudModelName = "Rawdata"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Nothing
        , bCrudModelDeleteFormDataJsonUrl = Nothing
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Nothing
        , bCrudModelEditFormTitleMsg = Nothing
        , bCrudModelDeleteFormTitleMsg = Nothing
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "EcmsR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "bytes"
              , bCrudFieldLabelDe = Just "Bytes"
              , bCrudFieldLabelEn = Just "Bytes"
              , bCrudFieldHsType = "ByteString"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            ]
        , bCrudModelTranslations = Nothing
        }



      , BCrudModel
        { bCrudModelName = "submission"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Nothing
        , bCrudModelDeleteFormDataJsonUrl = Nothing
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Nothing
        , bCrudModelEditFormTitleMsg = Nothing
        , bCrudModelDeleteFormTitleMsg = Nothing
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = ""
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "headline"
              , bCrudFieldLabelDe = Just "Headline"
              , bCrudFieldLabelEn = Just "Headline"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "subline"
              , bCrudFieldLabelDe = Just "Subline"
              , bCrudFieldLabelEn = Just "Subline"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "text"
              , bCrudFieldLabelDe = Just "Text"
              , bCrudFieldLabelEn = Just "Text"
              , bCrudFieldHsType = "Textarea"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            ]
        , bCrudModelTranslations = Nothing
        }



      , BCrudModel
        { bCrudModelName = "submissionfile"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Nothing
        , bCrudModelDeleteFormDataJsonUrl = Nothing
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Nothing
        , bCrudModelEditFormTitleMsg = Nothing
        , bCrudModelDeleteFormTitleMsg = Nothing
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = ""
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "submissionId"
              , bCrudFieldLabelDe = Just "Kunde"
              , bCrudFieldLabelEn = Just "Submission"
              , bCrudFieldHsType = "SubmissionId"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "rawdataId"
              , bCrudFieldLabelDe = Nothing
              , bCrudFieldLabelEn = Nothing
              , bCrudFieldHsType = "RawdataId"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "filename"
              , bCrudFieldLabelDe = Just "Dateiname"
              , bCrudFieldLabelEn = Just "Filename"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "mimetype"
              , bCrudFieldLabelDe = Just "MIME Type"
              , bCrudFieldLabelEn = Just "MIME Type"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "size"
              , bCrudFieldLabelDe = Just "Groesse"
              , bCrudFieldLabelEn = Just "Size"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            ]
        , bCrudModelTranslations = Nothing
        }




      , BCrudModel
        { bCrudModelName = "authorsubmission"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = False
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "AuthorR AuthorsubmissionListDataR"
        , bCrudModelEditFormDataJsonUrl = Just "AuthorR $ AuthorsubmissionDetailDataR authorsubmissionId"
        , bCrudModelDeleteFormDataJsonUrl = Just "AuthorR $ AuthorsubmissionListDataR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgAuthorsubmissionAddSubmission"
        , bCrudModelEditFormTitleMsg = Just "MsgAuthorsubmissionEditSubmission"
        , bCrudModelDeleteFormTitleMsg = Just "MsgAuthorsubmissionDeleteSubmission"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AuthorR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "headline"
              , bCrudFieldLabelDe = Just "Headline"
              , bCrudFieldLabelEn = Just "Headline"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "subline"
              , bCrudFieldLabelDe = Just "Subline"
              , bCrudFieldLabelEn = Just "Subline"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "text"
              , bCrudFieldLabelDe = Just "Text"
              , bCrudFieldLabelEn = Just "Text"
              , bCrudFieldHsType = "Textarea"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "textareaField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-textarea uk-form-small uk-width-5-6"
                        }
                      , BFieldAttr
                        { bFieldAttrKey = "rows", bFieldAttrValue = "10" }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-textarea uk-form-small uk-width-5-6"
                        }
                      , BFieldAttr
                        { bFieldAttrKey = "rows", bFieldAttrValue = "10" }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }

            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "submission", bTranslationDe = "Beitrag", bTranslationEn = "Submission" }
          , BTranslation { bTranslationKey = "submissions", bTranslationDe = "Beiträge", bTranslationEn = "Submissions" }
          , BTranslation { bTranslationKey = "addSubmission", bTranslationDe = "Beitrag hinzufügen", bTranslationEn = "Add submission" }
          , BTranslation { bTranslationKey = "editSubmission", bTranslationDe = "Beitrag bearbeiten", bTranslationEn = "Edit submission" }
          , BTranslation { bTranslationKey = "deleteSubmission", bTranslationDe = "Beitrag löschen", bTranslationEn = "Delete submission" }
          ]
        }



      , BCrudModel
        { bCrudModelName = "authorsubmissionfile"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = False
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "AuthorR $ AuthorsubmissionDetailDataR submissionId"
        , bCrudModelEditFormDataJsonUrl = Just "AuthorR $ AuthorsubmissionDetailDataR submissionId"
        , bCrudModelDeleteFormDataJsonUrl = Just "AuthorR $ AuthorsubmissionDetailDataR submissionId"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgAuthorsubmissionfileAddSubmissionfile"
        , bCrudModelEditFormTitleMsg = Just "MsgAuthorsubmissionfileEditSubmissionfile"
        , bCrudModelDeleteFormTitleMsg = Just "MsgAuthorsubmissionfileDeleteSubmissionfile"
        , bCrudModelParentHsType = Just "Submission"
        , bCrudModelFormRouteHsType = "AuthorR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "submissionId"
              , bCrudFieldLabelDe = Just "Kunde"
              , bCrudFieldLabelEn = Just "Submission"
              , bCrudFieldHsType = "SubmissionId"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "rawdataId"
              , bCrudFieldLabelDe = Nothing
              , bCrudFieldLabelEn = Nothing
              , bCrudFieldHsType = "RawdataId"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "filename"
              , bCrudFieldLabelDe = Just "Dateiname"
              , bCrudFieldLabelEn = Just "Filename"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "mimetype"
              , bCrudFieldLabelDe = Just "MIME Type"
              , bCrudFieldLabelEn = Just "MIME Type"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "size"
              , bCrudFieldLabelDe = Just "Groesse"
              , bCrudFieldLabelEn = Just "Size"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "file"
              , bCrudFieldLabelDe = Just "Datei"
              , bCrudFieldLabelEn = Just "File"
              , bCrudFieldHsType = "FileInfo"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "fileField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "submissionfiles", bTranslationDe = "Dateien", bTranslationEn = "Files" }
          , BTranslation { bTranslationKey = "addSubmissionfile", bTranslationDe = "Datei hinzufügen", bTranslationEn = "Add file" }
          , BTranslation { bTranslationKey = "editSubmissionfile", bTranslationDe = "Datei bearbeiten", bTranslationEn = "Edit file" }
          , BTranslation { bTranslationKey = "deleteSubmissionfile", bTranslationDe = "Datei löschen", bTranslationEn = "Delete file" }
          , BTranslation { bTranslationKey = "downloadSubmissionfile", bTranslationDe = "Datei runterladen", bTranslationEn = "Download file" }
          ]
        }





      , BCrudModel
        { bCrudModelName = "issue"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "EditorR IssueListDataR"
        , bCrudModelEditFormDataJsonUrl = Just "EditorR $ IssueDetailDataR issueId"
        , bCrudModelDeleteFormDataJsonUrl = Just "EditorR $ IssueListDataR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgIssueAddIssue"
        , bCrudModelEditFormTitleMsg = Just "MsgIssueEditIssue"
        , bCrudModelDeleteFormTitleMsg = Just "MsgIssueDeleteIssue"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "MyprojectR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "name"
              , bCrudFieldLabelDe = Just "Name"
              , bCrudFieldLabelEn = Just "Name"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }

            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "issue", bTranslationDe = "Ausgabe", bTranslationEn = "Issue" }
          , BTranslation { bTranslationKey = "issues", bTranslationDe = "Ausgaben", bTranslationEn = "Issues" }
          , BTranslation { bTranslationKey = "addIssue", bTranslationDe = "Ausgabe hinzufügen", bTranslationEn = "Add issue" }
          , BTranslation { bTranslationKey = "editIssue", bTranslationDe = "Ausgabe bearbeiten", bTranslationEn = "Edit issue" }
          , BTranslation { bTranslationKey = "deleteIssue", bTranslationDe = "Ausgabe löschen", bTranslationEn = "Delete issue" }
          ]
        }


      , BCrudModel
        { bCrudModelName = "editorsubmission"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = False
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "EditorR EditorsubmissionListDataR"
        , bCrudModelEditFormDataJsonUrl = Just "EditorR $ EditorsubmissionDetailDataR editorsubmissionId"
        , bCrudModelDeleteFormDataJsonUrl = Just "EditorR $ EditorsubmissionListDataR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgEditorsubmissionAddSubmission"
        , bCrudModelEditFormTitleMsg = Just "MsgEditorsubmissionEditSubmission"
        , bCrudModelDeleteFormTitleMsg = Just "MsgEditorsubmissionDeleteSubmission"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "EditorR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "headline"
              , bCrudFieldLabelDe = Just "Headline"
              , bCrudFieldLabelEn = Just "Headline"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "subline"
              , bCrudFieldLabelDe = Just "Subline"
              , bCrudFieldLabelEn = Just "Subline"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "text"
              , bCrudFieldLabelDe = Just "Text"
              , bCrudFieldLabelEn = Just "Text"
              , bCrudFieldHsType = "Textarea"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "textareaField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-textarea uk-form-small uk-width-5-6"
                        }
                      , BFieldAttr
                        { bFieldAttrKey = "rows", bFieldAttrValue = "10" }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-textarea uk-form-small uk-width-5-6"
                        }
                      , BFieldAttr
                        { bFieldAttrKey = "rows", bFieldAttrValue = "10" }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }

            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "submission", bTranslationDe = "Beitrag", bTranslationEn = "Submission" }
          , BTranslation { bTranslationKey = "submissions", bTranslationDe = "Beiträge", bTranslationEn = "Submissions" }
          , BTranslation { bTranslationKey = "addSubmission", bTranslationDe = "Beitrag hinzufügen", bTranslationEn = "Add submission" }
          , BTranslation { bTranslationKey = "editSubmission", bTranslationDe = "Beitrag bearbeiten", bTranslationEn = "Edit submission" }
          , BTranslation { bTranslationKey = "deleteSubmission", bTranslationDe = "Beitrag löschen", bTranslationEn = "Delete submission" }
          ]
        }



      , BCrudModel
        { bCrudModelName = "editorsubmissionfile"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = False
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "EditorR $ EditorsubmissionDetailDataR submissionId"
        , bCrudModelEditFormDataJsonUrl = Just "EditorR $ EditorsubmissionDetailDataR submissionId"
        , bCrudModelDeleteFormDataJsonUrl = Just "EditorR $ EditorsubmissionDetailDataR submissionId"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgEditorsubmissionfileAddSubmissionfile"
        , bCrudModelEditFormTitleMsg = Just "MsgEditorsubmissionfileEditSubmissionfile"
        , bCrudModelDeleteFormTitleMsg = Just "MsgEditorsubmissionfileDeleteSubmissionfile"
        , bCrudModelParentHsType = Just "Submission"
        , bCrudModelFormRouteHsType = "EditorR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "submissionId"
              , bCrudFieldLabelDe = Just "Kunde"
              , bCrudFieldLabelEn = Just "Submission"
              , bCrudFieldHsType = "SubmissionId"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "rawdataId"
              , bCrudFieldLabelDe = Nothing
              , bCrudFieldLabelEn = Nothing
              , bCrudFieldHsType = "RawdataId"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "filename"
              , bCrudFieldLabelDe = Just "Dateiname"
              , bCrudFieldLabelEn = Just "Filename"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "mimetype"
              , bCrudFieldLabelDe = Just "MIME Type"
              , bCrudFieldLabelEn = Just "MIME Type"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "size"
              , bCrudFieldLabelDe = Just "Groesse"
              , bCrudFieldLabelEn = Just "Size"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "file"
              , bCrudFieldLabelDe = Just "Datei"
              , bCrudFieldLabelEn = Just "File"
              , bCrudFieldHsType = "FileInfo"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "fileField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Just
          [ BTranslation { bTranslationKey = "submissionfiles", bTranslationDe = "Dateien", bTranslationEn = "Files" }
          , BTranslation { bTranslationKey = "addSubmissionfile", bTranslationDe = "Datei hinzufügen", bTranslationEn = "Add file" }
          , BTranslation { bTranslationKey = "editSubmissionfile", bTranslationDe = "Datei bearbeiten", bTranslationEn = "Edit file" }
          , BTranslation { bTranslationKey = "deleteSubmissionfile", bTranslationDe = "Datei löschen", bTranslationEn = "Delete file" }
          , BTranslation { bTranslationKey = "downloadSubmissionfile", bTranslationDe = "Datei runterladen", bTranslationEn = "Download file" }
          ]
        }




      ]










  , bContextActionModels =
      [
      ]


  , bContextGlobalTranslations =
    [ BTranslation { bTranslationKey = "home", bTranslationDe = "Home", bTranslationEn = "Home" }
    , BTranslation { bTranslationKey = "admin", bTranslationDe = "Admin", bTranslationEn = "Admin" }
    , BTranslation { bTranslationKey = "logout", bTranslationDe = "Logout", bTranslationEn = "Logout" }
    , BTranslation { bTranslationKey = "language", bTranslationDe = "Sprache", bTranslationEn = "Language" }
    , BTranslation { bTranslationKey = "myProfile", bTranslationDe = "Mein Profil", bTranslationEn = "My Profile" }
    , BTranslation { bTranslationKey = "editMyProfile", bTranslationDe = "Mein Profil bearbeiten", bTranslationEn = "Edit my profile" }
    , BTranslation { bTranslationKey = "reallyDelete", bTranslationDe = "Möchten sie wirklich löschen?", bTranslationEn = "Are you sure to delete?" }
    , BTranslation { bTranslationKey = "cancel", bTranslationDe = "Abbrechen", bTranslationEn = "Cancel" }

    , BTranslation { bTranslationKey = "editor", bTranslationDe = "Redakteur", bTranslationEn = "Editor" }
    , BTranslation { bTranslationKey = "reviewer", bTranslationDe = "Reviewer", bTranslationEn = "Reviewer" }
    , BTranslation { bTranslationKey = "author", bTranslationDe = "Autor", bTranslationEn = "Author" }
    ]
  }
