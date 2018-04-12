{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module HelloWorld where

import Servant ( QueryParam
               , PlainText
               , Get
               , JSON
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, ToJSON(..), (.=))

-- In this example we see how to output
-- json encoded data from the endpoints.
--

data Payload = Payload String String -- This is our json payload that we will output from the endpoint.

instance ToJSON Payload where
  toJSON (Payload itemOne itemTwo) = object ["itemOne" .= toJSON itemOne, "itemTwo" .= toJSON itemTwo]

type ServantType =  "payload" :> Get '[JSON] Payload

handlerPayload :: Handler Payload
handlerPayload = return $ Payload "itemOne" "itemTwo"

server :: Server ServantType
server = handlerPayload

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app
