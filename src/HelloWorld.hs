{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module HelloWorld where

import Servant
import Servant.Server
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class

-- "Hello world" of servant web application
-- In this we will create a web application with 
-- only two routes/endpoints, which are
--
-- /name, which return a hard coded name
-- /age, which returns a hard coded age

handlerName :: Handler String
handlerName = return "sras"

handlerAge :: Handler String
handlerAge = liftIO $ return "30"

-- 
-- The two functions that are supposed to 
-- handle these two endpoints can be seen above.
-- The handler functions should run in a 'Handler'
-- monad, which is something that is part of the Servant
-- This monad is an instance of MonadIO class, so you 
-- can do arbitrary IO in your handlers and can just liftIO
-- the whole thing into the Handler monad. You can see this
-- done in the 'age' handler.


type ServantType =  "name" :> Get '[PlainText] String
               :<|> "age" :> Get '[PlainText] String

-- Now we come to the most unique feature of servant
-- which is, the type of the webapplication
-- The rhs of the line above is a type, that is made
-- by joining types *representing* the indvidual handlers.
-- so `"name" :> Get '[PlainText] String` is the type
-- component that represent the first route that returns
-- the name, and `"age" :> Get '[PlainText] String` is
-- the second one, and both are combined using the `:<|>`
-- operator.
--
-- The actual urls for the above end up being "/name" and "/age".
-- if we wanted our name endpoint to show up at url
-- "/my/name" the type shoud be 
--
-- "my" :> "name" :> Get '[PlainText] String
--
-- NOTE: If you are thinking why this example
-- shows PlainText, instead of some Html representation
-- that is because Servant does not come with its own
-- html representation out of the box
--
-- https://hackage.haskell.org/package/servant-0.12/docs/Servant-API-ContentTypes.html#t:MimeRender
--
-- The provided content types are JSON, PlainText, FormUrlEncoded and OctetStream
-- We will see how to add an Html type in the next example

server :: Server ServantType
server = handlerName :<|> handlerAge

-- In the above lines, we combine the handlers
-- (just like we combined the types representing
-- handlers in the step before). Here too we
-- can use the :<|> operator to combine handlers.
-- Only here this is a regular operator, while
-- where we combined types, it was a type operator.

app :: Application
app = serve (Proxy :: Proxy ServantType) server

-- Here we make a regular wai application
-- from the servants representation of the
-- web app. If you are not familar with the `Proxy` stuff
-- it is something that is part of Data.Proxy module, and
-- is something that is commonly used in fancy typelevel
-- stuff.
--
-- Now that we have an wai `Application`,
-- we are out of the magical land of Servant and
-- back to the familiar world of wai.

mainFn :: IO ()
mainFn = run 4000 app

-- Now let us see how this app behaves
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
-- < Date: Sun, 12 Nov 2017 02:59:50 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- 30
--
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
-- < Date: Sun, 12 Nov 2017 03:17:44 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- sras
