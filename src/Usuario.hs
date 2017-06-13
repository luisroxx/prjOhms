{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Usuario where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Selects
import Data.Maybe

import Database.Persist.Postgresql
    
formUsu :: CasaId -> Int -> Form Usuario
formUsu casaId tpUsuario = renderDivs $ Usuario 
            <$> areq textField "" Nothing 
            <*> areq passwordField "" Nothing
            <*> pure tpUsuario
            <*> pure casaId
            
getCriarAutorizadoR :: Handler Html
getCriarAutorizadoR = do
            casaId <- selectCasaId
            let tpUsuario = 1
            listaUsu <- runDB $ selectList [UsuarioCasaId ==. casaId] [Asc UsuarioNmUsuario]
            (widget, enctype) <- generateFormPost $ formUsu casaId tpUsuario
            defaultLayout $ do
               addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
               addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
               addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
               widgetUsuario listaUsu CriarAutorizadoR enctype widget "Autorizado"
               -- ALTERAR O WIGET