{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module JsonOutput where

import Servant ( Get
               , JSON
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
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

-- curl -v http://127.0.0.1:4000/payload
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > GET /payload HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sun, 22 Jul 2018 08:57:34 GMT
-- < Server: Warp/3.2.23
-- < Content-Type: application/json;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- {"itemTwo":"itemTwo","itemOne":"itemOne"}
