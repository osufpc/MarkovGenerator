module Main where

---- imports for web server
import Data.Text (pack)
import Web.Spock
import Web.Spock.Config
import Control.Monad.IO.Class (liftIO)

import Source

---------------------------- Server --------------------------------------------
-- | if you're interested you can find this code explained in the Spock tutorial
-- here: https://www.spock.li/tutorials/rest-api

-- | Our api for the server, we don't require any databases, configuration, or
-- sessions
type Api = SpockM () () () ()

-- | An api action, yes this can be eta-reduced
type ApiAction a = SpockAction () () () a


main :: IO ()
main = do spockCfg <- defaultSpockCfg () PCNoDatabase ()
          runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "alice" $ do
    res <- liftIO $ alice >>= runFromSentences 100
    json $ case res of
             Left err -> pack err
             Right res' -> res'
