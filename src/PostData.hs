{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module PostData where

import Servant ( PlainText
               , JSON

               , Get
               , Post
               , ReqBody
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)

-- In this example, we see how we can accept
-- input in the request body, say a Json payload.

handlerName :: String -> Handler String
handlerName nameIn = return nameIn  -- Just output back the input string value

handlerAge :: Handler String
handlerAge = return "30"

-- In the code below, look at the `ReqBody '[JSON] String` part.
-- This is what enables our endpoint to recieve a String encoded as JSON
-- in the body of the request.
--
type ServantType =  "name" :> ReqBody '[JSON] String :> Post '[PlainText] String
               :<|> "age" :> Get '[PlainText] String

server :: Server ServantType
server = handlerName :<|> handlerAge

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app

-- Output
-- curl -v -H "Content-Type:application/json" -d "\"John\"" http://127.0.0.1:4000/name
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > POST /name HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- > Content-Type:application/json
-- > Content-Length: 6
-- >
-- * upload completely sent off: 6 out of 6 bytes
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Tue, 10 Apr 2018 16:27:57 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- John
