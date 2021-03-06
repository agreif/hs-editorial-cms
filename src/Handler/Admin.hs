{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Admin where

import Handler.Common
import Import
import qualified Database.Esqueleto as E
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI

getAdminHomeR :: Handler Html
getAdminHomeR = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{AdminR $ AdminDataR}")
                     \ })
                   |]

getAdminDataR :: Handler Value
getAdminDataR = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavAdmin
  jDataUsers <- userListJDataEnts
  jDataConfigs <- configListJDataEnts
  jDataRubricTypes <- rubricTypeListJDataEnts
  let pages =
        defaultDataPages
        { jDataPageAdmin =
            Just $ JDataPageAdmin
            { jDataPageAdminUsers = jDataUsers
            , jDataPageAdminConfigs = jDataConfigs
            , jDataPageAdminRubricTypes = jDataRubricTypes
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgAdmin <- localizedMsg MsgGlobalAdmin
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ AdminR AdminDataR
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ AdminR AdminHomeR
      , jDataHistoryStateTitle = msgAdmin
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ EcmsR HomeDataR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgAdmin
                               , jDataBreadcrumbItemDataUrl = currentDataUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ EcmsR $ LanguageDeR currentDataUrl
    , jDataLanguageEnUrl = urlRenderer $ EcmsR $ LanguageEnR currentDataUrl
    }

userListJDataEnts :: Handler [JDataUser]
userListJDataEnts = do
  urlRenderer <- getUrlRender
  userTuples <- runDB loadUserListTuples
  let jUserList = map (\(userEnt@(Entity userId _)) ->
                           JDataUser
                           { jDataUserEnt = userEnt
                           , jDataUserEditFormUrl = urlRenderer $ AdminR $ EditUserFormR userId
                           , jDataUserDeleteFormUrl = urlRenderer $ AdminR $ DeleteUserFormR userId
                           }
                        ) userTuples
  return jUserList

loadUserListTuples :: YesodDB App [(Entity User)]
loadUserListTuples = do
  tuples <- E.select $ E.from $ \(user) -> do
    E.orderBy [E.asc (user E.^. UserId)]
    return (user)
  return tuples

configListJDataEnts :: Handler [JDataConfig]
configListJDataEnts = do
  urlRenderer <- getUrlRender
  configTuples <- runDB loadConfigListTuples
  let jConfigList = map (\(configEnt@(Entity configId _)) ->
                           JDataConfig
                           { jDataConfigEnt = configEnt
                           , jDataConfigEditFormUrl = urlRenderer $ AdminR $ EditConfigFormR configId
                           }
                        ) configTuples
  return jConfigList

loadConfigListTuples :: YesodDB App [(Entity Config)]
loadConfigListTuples = do
  tuples <- E.select $ E.from $ \(config) -> do
    E.orderBy [E.asc (config E.^. ConfigId)]
    return (config)
  return tuples

rubricTypeListJDataEnts :: Handler [JDataRubricType]
rubricTypeListJDataEnts = do
  urlRenderer <- getUrlRender
  rubricTypeTuples <- runDB loadRubricTypeListTuples
  let jRubricTypeList = map (\(rubricTypeEnt@(Entity rubricTypeId _)) ->
                                 JDataRubricType
                                { jDataRubricTypeEnt = rubricTypeEnt
                                , jDataRubricTypeEditFormUrl = urlRenderer $ AdminR $ EditRubricTypeFormR rubricTypeId
                                , jDataRubricTypeDeleteFormUrl = urlRenderer $ AdminR $ DeleteRubricTypeFormR rubricTypeId
                                }
                              ) rubricTypeTuples
  return jRubricTypeList

loadRubricTypeListTuples :: YesodDB App [(Entity RubricType)]
loadRubricTypeListTuples = do
  tuples <- E.select $ E.from $ \(rubricType) -> do
    E.orderBy [E.asc (rubricType E.^. RubricTypeSortIndex)]
    return (rubricType)
  return tuples
