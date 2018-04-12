{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module HelloWorld where

import Servant ( QueryParam
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

-- "Hello world" of servant web application
-- In this we will create a web application with 
-- only two routes/endpoints, which are
--
-- /name, which return a hard coded name
-- /age, which returns a hard coded age

-- 
-- The two functions that are supposed to 
-- handle these two endpoints can be seen below.
--
handlerName :: Handler String
handlerName = return "sras"

handlerAge :: Handler String
handlerAge = liftIO $ (return "30" :: IO String) -- Using liftIO just to show that we can do arbitrary IO in the Handler

-- The handler functions for Servant should run in a 'Handler'
-- monad, which is something that is part of the Servant
-- This monad is an instance of MonadIO class, so you 
-- can do arbitrary IO in your handlers. You can see this
-- done in the 'age' handler, where we lift a value of type
-- 'IO String' to a value of type 'Handler String'

type ServantType =  "person" :> "name" :> Get '[PlainText] String
               :<|> "person" :> "age" :> Get '[PlainText] String

-- Now we come to the most unique feature of servant
-- which is, the webapplication represented as a type.
-- Each endpoint have its own type, and these types are
-- joined by the :<|> type operator, which ends up being
-- the type of the server that contains all the constituent
-- endpoints.
--
-- Let us start with a simple endpoint. Say we want this
-- endpoint to be avilable at url "/person/name" using GET method.
-- And say, we want this to return a plain text content to the browser.
--
-- The type of this endpoint can be
--
-- "person" :> "name" :> Get '[PlainText] String
--
-- Note that we had to separate the path segments using the :> operator
-- and it wouldn't work if we specify the path 
-- as "person/name" :> Get '[PlainText] String
--
-- Next is the 'Get' part, which decides the HTTP Method
-- by which this endpoint can be accessed. Servant provides
-- the following methods
--
--   GET
--   POST
--   HEAD
--   PUT
--   DELETE
--   TRACE
--   CONNECT
--   OPTIONS
--   PATCH
-- 
-- After the Method, we have this type level list
-- '[PlainText]. This configures the type of formats
-- that this endpoint could return. Right now we have
-- only PlainText in this list. So this endpoint can 
-- only output stuff in plain text format. The content
-- type header will contain "text/plain".
--
-- The available formats bundled with Servant are
-- PlainText, FormUrlEncoded, OctetStream and JSON
--
-- We will see how to add an Html type in the next example

server :: Server ServantType
server = handlerName :<|> handlerAge

-- In the above lines, we combine the handlers
-- (just like we combined the types representing
-- handlers in the step before). Here too we
-- can use the :<|> operator to combine handlers.
-- Only here this is a regular operator, that
-- operate of values.
--
app :: Application
app = serve (Proxy :: Proxy ServantType) server

-- Here we make a regular wai application
-- from the servants representation of the
-- web app. If you are not familar with the `Proxy` stuff
-- It is something that is part of Data.Proxy module, and
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
-- curl -v 127.0.0.1:4000/person/age
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
-- curl -v 127.0.0.1:4000/person/name
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
