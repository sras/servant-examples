{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module CustomPostMultipleFormats where

import Servant ( QueryParam
               , PlainText
               , JSON
               , MimeUnrender(..)
               , Accept(..)
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
import Data.Text.Lazy.Encoding as TE
import Data.Text.Lazy as TL
import Data.Aeson (FromJSON(..))

-- In this example, we see how we can accept data
-- in multiple formats, including custom ones. 

data ANewFormat

-- In the code below, look at the `ReqBody '[ANewFormat, JSON] String` part.
-- This is what enables our endpoint to recieve a value of type
-- `String` encoded as ANewFormat OR as JSON, in the body of the request.
-- The endpoint can accept both formats because `'[ANewFormat, JSON]` is a
-- type level list with two elements. ANewFormat and JSON.
-- If you were wondering, the JSON format is bundled in with Servant.
-- So we don't have to do additional stuff to make it work, but not so for our ANewFormat.
--
type ServantType =  "name" :> ReqBody '[ANewFormat, JSON] String :> Post '[PlainText] String
               :<|> "age" :> Get '[PlainText] String

instance Accept ANewFormat where
   contentType _ = "text/a-new-format"  -- This instance means that servant will use the decoding specific to ANewFormat as soon as it sees this content type ("text/a-new-format") in the incoming request.

instance MimeUnrender ANewFormat String where -- This instance implements the decoding of a bytestring that encodes some content in ANewFormat, into a target type (Which is String here)
  mimeUnrender _ bs = case TE.decodeUtf8' bs of
    Right x -> Right $ ("Decoded from ANewFormat - " ++ (T.unpack $ TL.toStrict x)) -- We just prefix the decoded text to differentiate it to show this was decoded using ANewFormat decoding logic.
    Left _ -> Left "Decoding error"

handlerName :: String -> Handler String 
handlerName nameIn = return nameIn  -- Just output back the input string value

handlerAge :: Handler String
handlerAge = return "30"

server :: Server ServantType
server = handlerName :<|> handlerAge

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app

-- Output 1 - See how the output differs when only the content type is changes, triggerring different decoding mechanisms.
--
-- curl -v -H "Content-Type:text/a-new-format" -d "\"John\"" http://127.0.0.1:4000/name
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > POST /name HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- > Content-Type:text/a-new-format
-- > Content-Length: 6
-- >
-- * upload completely sent off: 6 out of 6 bytes
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Wed, 11 Apr 2018 06:40:42 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- Decoded from ANewFormat - "John"
--
-- Output 2
--
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
-- < Date: Wed, 11 Apr 2018 06:41:07 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- John
