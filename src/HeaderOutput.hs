{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module HeaderOutput where

import Servant ( Header
               , PlainText
               , addHeader
               , Headers
               , Header
               , Get
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)

-- In this example we see how we can add a header to the response.
-- Look at the first endpoint type below. The types to denote the Header gets
-- added around the content, which is a String here.

type ServantType =  "singleHeader" :> Get '[PlainText] (Headers '[Header "CustomHeader" String] String)
               :<|> "multipleHeaders" :> Get '[PlainText] (Headers '[Header "CustomHeader2" String, Header "CustomHeader" String] String)
-- Compare this type with the type of the same endpoint with out the header in the line below
--
--   type ServantType =  "name" :> Get '[PlainText] String
--
-- So the type `String` at the end that represents the response body, is changed to `(Headers '[Header "CustomHeader" String] String)`
--
-- Now let us see how the handler and its type is changed to reflect this change

handlerWithHeader :: Handler (Headers '[Header "CustomHeader" String] String) -- Again, here, instead of `Handler String` (For handler without header), we have changed `String` to (Headers '[Header "CustomHeader" String] String)
handlerWithHeader = addHeader "CustomHeaderValue" <$> handlerWithoutHeader
-- Note how the "addHeader" function is applied to the response content (ie String, via fmap) instead of the whole handler. It all matches up with how the types was changed.
  where
    handlerWithoutHeader :: Handler String
    handlerWithoutHeader = return "Response Content"

-- Below is another handler that adds two Headers to the response. See how additional headers
-- gets added to the Handler type, and how the addHeader function is used.
-- You should be careful about the order in which is addHeader functions are called. Since
-- the value of both headers are Strings. If you want to guard against this mix up, use custom types
-- as Header values, instead of using Strings for all headers.

handlerWithMultipleHeader :: Handler (Headers '[Header "CustomHeader2" String, Header "CustomHeader" String] String) -- Again, here, instead of `Handler String` (For handler without header), we have changed `String` to (Headers '[Header "CustomHeader" String] String)
handlerWithMultipleHeader = (addHeader "CustomHeader2Value" . addHeader "CustomHeaderValue") <$> handlerWithoutHeader
-- Note how the "addHeader" function is applied to the response content (via fmap) intead of the whole handler. It all matches up with how the types was changed.
  where
    handlerWithoutHeader :: Handler String
    handlerWithoutHeader = return "Response Content"

server :: Server ServantType
server = handlerWithHeader :<|> handlerWithMultipleHeader

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app

-- curl -v http://127.0.0.1:4000/singleHeader
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > GET /singleHeader HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sun, 22 Jul 2018 08:49:01 GMT
-- < Server: Warp/3.2.23
-- < Content-Type: text/plain;charset=utf-8
-- < CustomHeader: CustomHeaderValue
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- Response Content
--
-- curl -v http://127.0.0.1:4000/multipleHeaders
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 4000 (#0)
-- > GET /multipleHeaders HTTP/1.1
-- > Host: 127.0.0.1:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sun, 22 Jul 2018 08:49:24 GMT
-- < Server: Warp/3.2.23
-- < Content-Type: text/plain;charset=utf-8
-- < CustomHeader2: CustomHeader2Value
-- < CustomHeader: CustomHeaderValue
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- Response Content
