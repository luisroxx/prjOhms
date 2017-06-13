{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where
import Foundation
import Yesod
import Usuario
import Handlers
import Front

-- Application
mkYesodDispatch "Sitio" resourcesSitio
