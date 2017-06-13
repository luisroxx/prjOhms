{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Yesod
import Yesod.Static
import Data.Text
import Data.Time.Calendar
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Casa json
    nmCasa Text
    deriving Show

Usuario json
    nmUsuario Text
    cdPassword Text
    tpUsuario Int
    casaId CasaId
    UniqueEmail nmUsuario
    
Ambiente json
    nmAmbiente Text
    casaId CasaId
    deriving Show
    
Preco json
    qtPreco Double
    deriving Show

Consumo json
    dtConsumo Day
    qtConsumo Double
    precoId PrecoId
    ambienteId AmbienteId
    deriving Show
    UNIQUEConsumo dtConsumo ambienteId
|]

staticFiles "static"

mkYesodData "Sitio" $(parseRoutesFile "config/routes")

mkMessage "Sitio" "messages" "pt-BR"

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just LoginR
    isAuthorized HomeR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized ConsumoR _ = return Authorized
    isAuthorized PrecoR _ = isAdmin
    isAuthorized CriarAutorizadoR _ = isAdmin
    isAuthorized CriarAmbienteR _ = isAdmin
    isAuthorized _ _ = isUser

isAdmin = do
    mi <- lookupSession "_ID"
    mu <- lookupSession "_USER"
    return $ case mi of
        Nothing -> do 
            case mu of
              Nothing -> AuthenticationRequired
              Just _ -> Unauthorized "Soh o admin acessa aqui!"
        Just "admin" -> Authorized

isUser = do
    mu <- lookupSession "_USER"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage
    
widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = $(whamletFile "templates/form.hamlet")

widgetAmbiente :: [Entity Ambiente] -> Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetAmbiente listaAmb x enctype widget y = $(whamletFile "templates/ambiente/new.hamlet")

widgetListarAmbiente :: [Entity Ambiente] -> Widget
widgetListarAmbiente listaAmb = $(whamletFile "templates/ambiente/listar.hamlet")

widgetUsuario :: [Entity Usuario] -> Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetUsuario listaUsu x enctype widget y = $(whamletFile "templates/usuario/new.hamlet")

widgeHome :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgeHome x enctype widget y = $(whamletFile "templates/index.hamlet")

widgetVisuAmbiente :: Ambiente -> Casa -> Double -> Widget
widgetVisuAmbiente ambiente casa  consumo = $(whamletFile "templates/ambiente/edit.hamlet")