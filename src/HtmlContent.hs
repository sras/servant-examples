{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HtmlContent where

import Servant
import Servant.Server
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class

import Data.ByteString.Lazy.Char8 as C

handlerName :: Handler String
handlerName = return "sras"

handlerAge :: Handler Int
handlerAge = liftIO $ return 30

data HTML 
-- This is our type to represent Html content. No constructor
-- is required because we wont be creating value of this type

-- The following instance is what required
-- to make the HTML type appear in place of
-- content type in Servant urls.
instance Accept HTML where
   contentType _ = "text/html" 

-- The below instance enables endpoints
-- with HTML content type to be handled
-- by handlers that return a String. The
-- next instnace enables them to be handled
-- by handlers that return an Int.

instance MimeRender HTML String  where 
   mimeRender _ val = C.pack val

instance MimeRender HTML Int  where 
   mimeRender _ val = C.pack $ show $ val

-- The above instance enables endpoints
-- with HTML content type to be handled
-- by handlers that return an Int
--

type ServantType =  "name" :> Get '[HTML] String
               :<|> "age" :> Get '[HTML] Int

server :: Server ServantType
server = handlerName :<|> handlerAge

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app

-- Now let us see how the app behaves
--
--
-- curl -v 127.0.0.1:4000/age
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > GET /age HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sun, 12 Nov 2017 13:16:50 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: text/html
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- 30
