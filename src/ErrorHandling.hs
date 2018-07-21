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
import Control.Monad.Error.Class

import Servant.Server.Internal.ServantErr
-- ^^ This is the module from which the errors that we can throw are imported.
-- For some reason this is a not really mentioned in the documentation and is marked an Internal module.
-- To see all the possible types of errors we can throw or how to make our custom errors, refer the following.
-- http://hackage.haskell.org/package/servant-server-0.14.1/docs/Servant-Server-Internal-ServantErr.html

handlerWithError :: Handler String
handlerWithError = if True -- If there was an error ?
  then throwError err500  -- We throw error here. Read more about it below.
  else return "sras" -- else return result.

-- The function err500 is part of Servant and returns a value of type 'ServantErr'.
-- The throwError function is not part of Servant library.
-- We can use it to throw errors of type `ServantErr` in the `Handler` monad
-- only because of the typeclass instance `MonadError ServantErr Handler`.
-- You can see it in the documentation page.
-- http://hackage.haskell.org/package/servant-server-0.14.1/docs/Servant-Server-Internal-ServantErr.html

handlerAge :: Handler String
handlerAge = return "30"

type ServantType =  "person" :> "name" :> Get '[PlainText] String
               :<|> "person" :> "age" :> Get '[PlainText] String


server :: Server ServantType
server = handlerWithError :<|> handlerAge

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app
--
-- curl -v  http://localhost:4000/person/name
-- *   Trying 127.0.0.1...
-- * Connected to localhost (127.0.0.1) port 4000 (#0)
-- > GET /person/name HTTP/1.1
-- > Host: localhost:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 500 Internal Server Error
-- < Transfer-Encoding: chunked
-- < Date: Sat, 21 Jul 2018 16:59:19 GMT
-- < Server: Warp/3.2.23
-- <
-- * Connection #0 to host localhost left intact
