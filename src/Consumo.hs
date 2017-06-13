{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
 
module Consumo where
import Yesod
import Foundation
import Front
import Selects
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text hiding (map)
import Data.Time.Calendar
import GHC.Generics

import Database.Persist.Postgresql
import Network.HTTP.Types.Status


data ConsumoTemp = ConsumoTemp {dtConsumo :: Day,
                                qtConsumo :: Double,
                                precoId :: PrecoId,
                                ambienteId :: AmbienteId} deriving Generic

instance ToJSON ConsumoTemp
instance FromJSON ConsumoTemp

postConsumoR :: Handler TypedContent
postConsumoR = do
    consumo <- requireJsonBody :: Handler ConsumoTemp
    temCons <- runDB $ selectFirst [ConsumoDtConsumo ==. dtConsumo consumo, ConsumoAmbienteId ==. ambienteId consumo] []
    case temCons of
        Nothing -> do
            cid <- runDB $ insert $ fazConsumo (dtConsumo consumo) (qtConsumo consumo) (precoId consumo) (ambienteId consumo)
            sendStatusJSON created201 (object ["resp" .= (fromSqlKey cid)])
        Just _ -> do
            cid <-runDB $ updateWhere [ConsumoDtConsumo==. dtConsumo consumo] [ConsumoQtConsumo+=. qtConsumo consumo]
            sendResponseStatus status200 ("UPDATED" :: Text)
            
fazConsumo :: Day -> Double -> PrecoId -> AmbienteId -> Consumo
fazConsumo dt qt pr amb = Consumo dt qt pr amb

getConsumoCasaR :: Handler Html
getConsumoCasaR = do
        consumoCasa <- selectConsumoCasa -- isso aqui é Double
        casaId <- selectCasaId -- Casa ID
        casa     <- runDB $ get404 casaId
        preco <- selectPreco -- Double
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           widgetVisuConsumo casa preco consumoCasa