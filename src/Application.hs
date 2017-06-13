{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where
import Foundation
import Yesod
import Usuario
import Login
import Selects

-- Application
mkYesodDispatch "Sitio" resourcesSitio
