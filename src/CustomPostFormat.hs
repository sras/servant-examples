{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module CustomPostFormat where

import Servant ( PlainText
               , MimeUnrender(..)
               , Accept(..)
               , Post
               , ReqBody
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Data.Text as T (unpack)
import Data.Text.Lazy.Encoding as TE (decodeUtf8')
import Data.Text.Lazy as TL (toStrict)

-- In this example, we see how we can accept data
-- in any custom format. Earlier example, we were accepting
-- data in JSON format, which Servant has built in support for.

data ANewFormat -- This is our new format that we will be using along with JSON. We won't be needing a constructor since we won't be dealing with values of this type.
                -- The sole purpose of this type is to enable the Haskell type system to select the proper decoding/encoding and content type generation
                -- functions inside proper typeclass instances, which is why we don't need a constructor.

-- In the code below, you can see the type of the two end points.
-- Look at the `ReqBody '[ANewFormat] String` part, in the first one.
-- This is what enables our endpoint to recieve a value of type
-- `String` encoded as ANewFormat, in the body of the request.
-- The MimeUnrender instance also defines how some bytestring
-- encoded as ANewFormat can be decoded into a String.
type ServantType =  "name-in-new-format" :> ReqBody '[ANewFormat] String :> Post '[PlainText] String
               :<|> "name" :> ReqBody '[PlainText] String :> Post '[PlainText] String

instance Accept ANewFormat where
   contentType _ = "text/a-new-format"  -- This instance means that servant will use the decoding specific to ANewFormat as soon as it sees this content type ("text/a-new-format") in the incoming request.

instance MimeUnrender ANewFormat String where -- This instance implements the decoding of a bytestring that encodes some content in ANewFormat, into a target type (Which is String here)
  mimeUnrender _ bs = case TE.decodeUtf8' bs of
    Right x -> Right $ ("Decoded from ANewFormat - " ++ (T.unpack $ TL.toStrict x)) -- We just prefix the decoded text to differentiate it to show this was decoded using ANewFormat decoding logic.
    Left _ -> Left "Decoding error"

handlerName :: String -> Handler String
handlerName nameIn = return nameIn -- Just output back the input string value

server :: Server ServantType
server = handlerName :<|> handlerName  -- We can use same handler for both endpoints, because they only differ in input encoding.

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app

-- Output - See how the output differs when only the content type is changes, triggerring different decoding mechanisms.
--
-- $ curl -v -H "Content-Type:text/a-new-format" -d "\"John\"" http://127.0.0.1:4000/name-in-new-format
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > POST /name-in-new-format HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- > Content-Type:text/a-new-format
-- > Content-Length: 6
-- >
-- * upload completely sent off: 6 out of 6 bytes
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sun, 22 Jul 2018 07:37:15 GMT
-- < Server: Warp/3.2.23
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- Decoded from ANewFormat - "John"
--
-- $ curl -v -H "Content-Type:text/plain;charset=utf-8" -d "John" http://127.0.0.1:4000/name
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > POST /name HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- > Content-Type:text/plain;charset=utf-8
-- > Content-Length: 4
-- >
-- * upload completely sent off: 4 out of 4 bytes
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sun, 22 Jul 2018 07:40:17 GMT
-- < Server: Warp/3.2.23
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- Johns
