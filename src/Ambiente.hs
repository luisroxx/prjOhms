{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Ambiente where
import Yesod
import Foundation
import Front
import Selects
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Data.Maybe

import Database.Persist.Postgresql

formAmb :: CasaId -> Form Ambiente
formAmb casaId = renderTable $ Ambiente 
            <$> areq nmAmbienteField "" Nothing 
            <*> pure casaId
            
nmAmbienteField :: Field Handler Text
nmAmbienteField = Field
    { fieldParse = \rawVals _ ->
                 case rawVals of
                   [a] -> return $ Right $ Just a
                   [] -> return $ Right Nothing
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
        <div .form-group> 
            <h4>Nome:
            <input .form-control  placeholder="Insira o nome do ambiente:" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField>
        |]
    , fieldEnctype = UrlEncoded
    }

getCriarAmbienteR :: Handler Html
getCriarAmbienteR = do
        casaId <- selectCasaId
        listaAmb <- runDB $ selectList [] [Asc AmbienteNmAmbiente]
        (widget, enctype) <- generateFormPost $ formAmb casaId
        --defaultLayout $ widgetForm CriarAmbienteR enctype widget "Criar Ambiente"
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           widgetAmbiente listaAmb CriarAmbienteR enctype widget "Ambiente"