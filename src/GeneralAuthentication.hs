{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module GeneralAuthentication where

import Servant ( QueryParam
               , PlainText
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
import Control.Monad.IO.Class (liftIO)

import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

data User = User

lookupUser :: Handler User
lookupUser = return User

authHandler :: AuthHandler Request User
authHandler = undefined

handlerName :: User -> Handler String
handlerName user = return "sras"

handlerAge :: Handler String
handlerAge = liftIO $ (return "30" :: IO String) -- Using liftIO just to show that we can do arbitrary IO in the Handler

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
