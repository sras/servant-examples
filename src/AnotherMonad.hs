{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module HelloWorld where

import Servant ( QueryParam
               , PlainText
               , Get
               , ServerT
               , hoistServer -- Servant function to make a custom monad with with Servant.
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader

type MyServerType =  "person" :> "name" :> Get '[PlainText] String  -- The endpoint types does not have to change to accomodate a different monad
               :<|> "person" :> "age" :> Get '[PlainText] String

handlerName :: Reader String String  -- These two are our two handlers. But instead of returning a `Handler`, it returns a Reader. We will see how these handlers can be made to work with Servant.
handlerName = return "sras"

handlerAge :: Reader String String
handlerAge = return "10"

api :: Proxy MyServerType
api = Proxy

readerServer :: ServerT MyServerType (Reader String) -- Endpoints are combined together as before. Here the endpoint types are still our custom monad. The Reader monad.
readerServer = handlerName :<|> handlerAge           -- At the next step, we will convert this consolidated server, into something that Servant can handle.

handlerServer :: ServerT MyServerType Handler  -- This code is the important part where we convert a value of type `ServerT MyServerType (Reader String)` to a value of type `ServerT MyServerType Handler`, using the hoistServer function from Servant.
handlerServer = hoistServer api readerToHandler readerServer
  where
    readerToHandler :: Reader String x -> Handler x  -- This code just extracts the value from our custom monads (Reader here) and wraps it in the Handler monad.
    readerToHandler r = return $ runReader r "reader env"

app :: Application
app = serve api handlerServer

mainFn :: IO ()
mainFn = run 4000 app

-- curl -v  http://localhost:4000/person/name
-- *   Trying 127.0.0.1...
-- * Connected to localhost (127.0.0.1) port 4000 (#0)
-- > GET /person/name HTTP/1.1
-- > Host: localhost:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sat, 21 Jul 2018 17:00:44 GMT
-- < Server: Warp/3.2.23
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host localhost left intact
-- sras
