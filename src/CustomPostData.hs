{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module CustomPostData where

import Servant ( QueryParam
               , PlainText
               , JSON
               , FromHttpApiData(..)
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
import Control.Monad.IO.Class (liftIO)
import Data.Text as T
import Data.Aeson (FromJSON(..))

-- In this example, we see how we can recive data in the request
-- body in a custom format.

data NameWrapper = NameWrapper { getName :: String } -- This the type that our handler expects.

-- In the code below, look at the `ReqBody '[JSON] NameWrapper` part.
-- This (along with the FromJSON instance) is what enables our endpoint to recieve a value of type
-- `NameWrapper` encoded as JSON, in the body of the request.
--
type ServantType =  "name" :> ReqBody '[JSON] NameWrapper :> Post '[PlainText] String
               :<|> "age" :> Get '[PlainText] String

-- To make this work, NameWrapper should have an instance of FromJSON
--
instance FromJSON NameWrapper where
  parseJSON v = NameWrapper <$> (parseJSON v)

handlerName :: NameWrapper -> Handler String
handlerName (NameWrapper nameIn) = return nameIn  -- Just output back the input string value

handlerAge :: Handler String
handlerAge = return "30"

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
