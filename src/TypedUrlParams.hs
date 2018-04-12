{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TypedUrlParams where

import Servant ( QueryParam
               , PlainText
               , FromHttpApiData(..)
               , Get
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Data.Text as T

-- In this example we look at how
-- we can recieve parameters via 
-- url and have them automatically
-- converted to value of a custom type.

data Wrapper = Wrapper String -- This is our custom type.

instance FromHttpApiData Wrapper where -- This the the instance that enables the auto conversion from Text in a url to the value of a custom type that the handler expects.
  parseQueryParam i = Right (Wrapper $ T.unpack i)

handlerName :: Maybe Wrapper -> Handler String -- The handler. Look at the first argument, it is of the Wrapper type (our custom type).
handlerName nameIn = case nameIn of 
  Just (Wrapper name) -> return name
  Nothing -> return "Anonymous"

handlerAge :: Handler String
handlerAge = return "30"

type ServantType =  "name" :> QueryParam "input" Wrapper :> Get '[PlainText] String -- Look at the Wrapper type mentioned in `QueryParam "input" Wrapper`.
               :<|> "age" :> Get '[PlainText] String

server :: Server ServantType
server = handlerName :<|> handlerAge

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app

-- Output
--
-- curl -v http://127.0.0.1:4000/name?input=John
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > GET /name?input=John HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Tue, 10 Apr 2018 15:11:36 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- John
