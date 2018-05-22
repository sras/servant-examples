{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module UrlParams where

import Servant ( QueryParam
               , Capture
               , PlainText
               , Get
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)

-- In this example we look at how
-- we can recieve parameters via 
-- url. Here we create an endpoint
-- that echos back the name we pass
-- as a url parameter

-- Right below is our handler for the route
--
-- `/name?input=<input-name>`
--
-- The type of this route is `"name" :> QueryParam "input" String :> Get '[PlainText] String`
-- The QueryParam segment declares the argument we expect in the url
-- `QueryParam "input" String` means we expect a value in "input" key
-- and we need expect it as a String in the handler.
-- This will be passed on to our handler function as an argument.
-- You can also see that it is a Maybe type in handler since
-- we also need to handle cases where the route is accessed
-- without the specific parameter
--
-- We have another route below that is of the segmented format.
-- For example /name/john.
--

handlerName :: Maybe String -> Handler String
handlerName nameIn = case nameIn of 
  Just name -> return name
  Nothing -> return "Anonymous"

handlerRequiredName :: String -> Handler String
handlerRequiredName nameIn = return nameIn

type ServantType =  "name" :> QueryParam "input" String :> Get '[PlainText] String -- /name?input=john
               :<|> "name" :> Capture "input" String :> Get '[PlainText] String -- /name/John

server :: Server ServantType
server = handlerName :<|> handlerRequiredName

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app

-- Output

-- curl -v 127.0.0.1:4000/name
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > GET /name HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sun, 12 Nov 2017 15:07:16 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- Anonymous
--
-- $ curl -v 127.0.0.1:4000/name?input=John
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > GET /name?input=John HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sun, 12 Nov 2017 15:07:29 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- John
--
-- curl -v 127.0.0.1:4000/name/John
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > GET /requiredName/John HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Tue, 17 Apr 2018 12:57:07 GMT
-- < Server: Warp/3.2.18.2
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- John
