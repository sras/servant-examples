{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module GeneralAuthentication where

import Servant ( PlainText
               , AuthProtect
               , Get
               , Context((:.), EmptyContext)
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serveWithContext)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Request)

import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

data User = User

lookupUser :: Request -> Handler User
lookupUser = undefined -- Actual authenticating function

authHandler :: AuthHandler Request User
authHandler = mkAuthHandler lookupUser

handlerName :: User -> Handler String
handlerName user = return "sras"

handlerAge :: Handler String
handlerAge = return "30"

type instance AuthServerData (AuthProtect "Example Auth Realm") = User

type ServantType = AuthProtect "Example Auth Realm" :> "person" :> "name" :> Get '[PlainText] String
               :<|> "person" :> "age" :> Get '[PlainText] String

server :: Server ServantType
server = handlerName :<|> handlerAge

app :: Application
app = serveWithContext (Proxy :: Proxy ServantType) ctx server
  where
    ctx = authHandler :. EmptyContext

mainFn :: IO ()
mainFn = run 4000 app
