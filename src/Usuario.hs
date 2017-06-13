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
            <$> areq nmUsuarioField "" Nothing 
            <*> areq cdPasswordField "" Nothing
            <*> pure tpUsuario
            <*> pure casaId
            
nmUsuarioField :: Field Handler Text
nmUsuarioField = Field
    { fieldParse = \rawVals _ ->
                 case rawVals of
                   [a] -> return $ Right $ Just a
                   [] -> return $ Right Nothing
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
        <div .form-group> 
            <h4>Email:
            <input .form-control  placeholder="Insira o email do usuário:" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=emailField>
        |]
    , fieldEnctype = UrlEncoded
    }
    
cdPasswordField :: Field Handler Text
cdPasswordField = Field
    { fieldParse = \rawVals _ ->
                 case rawVals of
                   [a] -> return $ Right $ Just a
                   [] -> return $ Right Nothing
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
        <div .form-group> 
            <h4>Senha:
            <input .form-control  placeholder="Insira a senha do usuário:" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=password>
        |]
    , fieldEnctype = UrlEncoded
    }
            
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
               
getListarUsuarioR :: Handler Html
getListarUsuarioR = do
                    casaId <- selectCasaId
                    listaUsu <- runDB $ selectList [UsuarioCasaId ==. casaId] []
                    defaultLayout $ do 
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                    widgetListarUsuario listaUsu
                    
postApagarUsuarioR :: UsuarioId -> Handler Html
postApagarUsuarioR uid = do
                uid <- runDB $ delete uid
                defaultLayout $ do
                   addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                   addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                   addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                   [whamlet| 
                        <div .form-group>
                        <h1> Usuário deletado!
                        <a href=@{ListarUsuarioR}>
                          <button .btn .btn-primary type="submit">Voltar
                |]                    
                
postCriarAutorizadoR :: Handler Html
postCriarAutorizadoR = do
            casaId <- selectCasaId
            let tpUsuario = 1 
            ((res, _), _) <- runFormPost $ formUsu casaId tpUsuario
            case res of
                    FormSuccess (formUsu) -> do
                       idx <- runDB $ insert formUsu
                       defaultLayout $ do
                           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                           [whamlet| 
                                <div .form-group>
                                <h1> Usuário autorizado inserido com sucesso!
                                <a href=@{CriarAutorizadoR}>
                                  <button .btn .btn-primary type="submit">Voltar
                        |]
                    _ -> redirect CriarAutorizadoR     
                    
formNewUsu :: Form (Text, Text)
formNewUsu  = renderDivs $ (,) <$> 
            areq nmUsuarioField "" Nothing <*>
            areq cdPasswordField "" Nothing        
                   
getHomeR :: Handler Html
getHomeR = do
            (widget, enctype) <- generateFormPost formNewUsu
            defaultLayout $ do
               addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
               addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
               addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
               widgeHome HomeR enctype widget "Autorizado"                    