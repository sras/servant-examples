{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CustomOutput where

import Servant ( Get
               , MimeRender(..)
               , Accept(..)
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import qualified Data.Text.Lazy as T (pack)
import qualified Data.Text.Lazy.Encoding as TE (encodeUtf8)

-- In this example we see how to output
-- data using a custom encoding.

data ANewFormat

instance Accept ANewFormat where  -- Accept instance is required for output encoding as well.
  contentType _ = "text/a-new-format"

instance MimeRender ANewFormat String where -- This is where the actual encoding happens
  mimeRender _ s = TE.encodeUtf8 $ T.pack ("ANewFormat:" ++ s)

type ServantType =  "name" :> Get '[ANewFormat] String

handlerName :: Handler String
handlerName = return "sras"

server :: Server ServantType
server = handlerName

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app

-- $ curl -v  http://localhost:4000/name
-- *   Trying 127.0.0.1...
-- * Connected to localhost (127.0.0.1) port 4000 (#0)
-- > GET /name HTTP/1.1
-- > Host: localhost:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sun, 22 Jul 2018 07:22:10 GMT
-- < Server: Warp/3.2.23
-- < Content-Type: text/a-new-format
-- <
-- * Connection #0 to host localhost left intact
-- ANewFormat:sras
