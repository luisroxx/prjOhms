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
 
module Selects where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text hiding (replace, map)
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock

import Database.Persist.Postgresql
import Network.HTTP.Types.Status

selectCasaId :: Handler CasaId
selectCasaId = do
        username <- lookupSession "_USER"
        temUsu <- runDB $ selectFirst [UsuarioNmUsuario ==. fromJust(username)] []
        casa <- return $ fromJust $ fmap (usuarioCasaId . entityVal) $ temUsu
        return $ casa --RETORNA UM  KEY CASA
        
selectPreco :: Handler Double
selectPreco = do
        price <- runDB $ selectFirst [PrecoId ==. toSqlKey(0)] []
        valPrice <- return $ fromJust $ fmap (precoQtPreco . entityVal) $ price
        return $ valPrice --RETORNA O QT PRECO
        
selectConsumoAmbiente :: AmbienteId -> Handler [Double]
selectConsumoAmbiente aid = do
    consumo <- runDB $ selectList [ConsumoAmbienteId ==. aid] []
    consumoAmbiente <- return $ map (consumoQtConsumo . entityVal) consumo
    return $ consumoAmbiente --RETORNA UMA LISTA DE CONSUMOS DO AMBIENTE