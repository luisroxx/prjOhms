{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Login where
import Yesod
import Foundation
import Front
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Data.Maybe

import Database.Persist.Postgresql

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) <$>
             areq textField "" Nothing <*>
             areq passwordField "" Nothing 
            

getLoginR :: Handler Html
getLoginR = do
            usr <- lookupSession "_USER"
            case usr of
                Nothing -> do
                    (widget, enctype) <- generateFormPost formLogin
                    defaultLayout $ do
                     addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                     addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                     addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                     widgetForm LoginR enctype widget "Login page" 
                Just _ -> do
                    usrId <- lookupSession "_ID"
                    case usrId of
                        Nothing ->redirect CriarAmbienteR
                        Just _ -> redirect CriarAmbienteR
                        
postLoginR :: Handler Html
postLoginR = undefined

postLogoutR :: Handler Html
postLogoutR = undefined