{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HtmlContent where

import Servant ( Get
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               , Accept(..)
               , MimeRender(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy.Char8 as C (pack)

-- In this example, we add an Html type and make endpoints
-- with this content type return a content type header with
-- "text/html" in it.

data HTML -- Here is our HTML type that we will use in the type of the endpoint.
          -- We don't need a constructor here since we ll ever have to deal with a value of this type.

instance Accept HTML where         -- This instance is what makes the endpoints with this content type
   contentType _ = "text/html"     -- return content with a content type header with "text/html" in it.

instance MimeRender HTML String  where  -- This instance where we define how a value of type string is
   mimeRender _ val = C.pack val        -- is encoded as an html value. Note that we are not converting
                                        -- the string to an value of type HTML, but just to a Bytestring that
                                        -- represents the HTML encoding. As I said earlier, we won't ever
                                        -- have to deal with a value of type HTML

instance MimeRender HTML Int  where        -- Same as before. This instance defines how an Int will be converted
   mimeRender _ val = C.pack $ show $ val  -- to a bytestring for endpoints with HTML content type.

type ServantType =  "name" :> Get '[HTML] String
               :<|> "age" :> Get '[HTML] Int

handlerName :: Handler String
handlerName = return "sras"

handlerAge :: Handler Int
handlerAge = return 30

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
