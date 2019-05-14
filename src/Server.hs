module Server where

import Web.Spock
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
