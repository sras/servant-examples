{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module BasicAuthentication where

import Servant ( PlainText
               , BasicAuth
               , BasicAuthCheck(..)
               , BasicAuthData(..)
               , BasicAuthResult(..)
               , Get
               , Context((:.), EmptyContext)
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serveWithContext)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)

data User = User

handlerName :: User -> Handler String
handlerName _ = return "sras"

handlerAge :: Handler String
handlerAge = liftIO (return "30" :: IO String) -- Using liftIO just to show that we can do arbitrary IO in the Handler

type ServantType = BasicAuth "Example Auth Realm" User :> "person" :> "name" :> Get '[PlainText] String
               :<|> "person" :> "age" :> Get '[PlainText] String

server :: Server ServantType
server = handlerName :<|> handlerAge

myAuthCheck :: BasicAuthData -> IO (BasicAuthResult User)
myAuthCheck (BasicAuthData u p) = return $ if u == "sras" && p == "sras_password" then Authorized User else BadPassword

-- ^^ The above function is the one that actually check the username
-- and password and return an value that indicate the status of authentication. Look in the 'app' function
-- to see how it is used. The value returned can be one of
--
-- Unauthorized
-- BadPassword
-- NoSuchUser
-- Authorized usr
--
-- Refer http://hackage.haskell.org/package/servant-server-0.14.1/docs/Servant-Server-Internal-BasicAuth.html

app :: Application
app = serveWithContext (Proxy :: Proxy ServantType) ctx server
  where
    ctx = (BasicAuthCheck myAuthCheck) :. EmptyContext

mainFn :: IO ()
mainFn = run 4000 app
--
-- $  curl -v  http://localhost:4000/person/name
-- *   Trying 127.0.0.1...
-- * Connected to localhost (127.0.0.1) port 4000 (#0)
-- > GET /person/name HTTP/1.1
-- > Host: localhost:4000
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 401 Unauthorized
-- < Transfer-Encoding: chunked
-- < Date: Sat, 21 Jul 2018 17:02:11 GMT
-- < Server: Warp/3.2.23
-- < WWW-Authenticate: Basic realm="Example Auth Realm"
-- <
-- * Connection #0 to host localhost left intact
--
-- $ curl -v  -u sras:password http://localhost:4000/person/name
-- *   Trying 127.0.0.1...
-- * Connected to localhost (127.0.0.1) port 4000 (#0)
-- * Server auth using Basic with user 'sras'
-- > GET /person/name HTTP/1.1
-- > Host: localhost:4000
-- > Authorization: Basic c3JhczpwYXNzd29yZA==
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 401 Unauthorized
-- < Transfer-Encoding: chunked
-- < Date: Sat, 21 Jul 2018 17:08:20 GMT
-- < Server: Warp/3.2.23
-- * Authentication problem. Ignoring this.
-- < WWW-Authenticate: Basic realm="Example Auth Realm"
-- <
-- * Connection #0 to host localhost left intact
--
-- $ curl -v  -u sras:sras_password http://localhost:4000/person/name
-- *   Trying 127.0.0.1...
-- * Connected to localhost (127.0.0.1) port 4000 (#0)
-- * Server auth using Basic with user 'sras'
-- > GET /person/name HTTP/1.1
-- > Host: localhost:4000
-- > Authorization: Basic c3JhczpzcmFzX3Bhc3N3b3Jk
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sat, 21 Jul 2018 17:08:27 GMT
-- < Server: Warp/3.2.23
-- < Content-Type: text/plain;charset=utf-8
-- <
-- * Connection #0 to host localhost left intact
-- sras
