{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module BasicAuthentication where

import Servant ( QueryParam
               , PlainText
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
handlerName user = return "sras"

handlerAge :: Handler String
handlerAge = liftIO $ (return "30" :: IO String) -- Using liftIO just to show that we can do arbitrary IO in the Handler


type ServantType = BasicAuth "Example Auth Realm" User :> "person" :> "name" :> Get '[PlainText] String
               :<|> "person" :> "age" :> Get '[PlainText] String

server :: Server ServantType
server = handlerName :<|> handlerAge

myAuthCheck :: BasicAuthData -> IO (BasicAuthResult User)
myAuthCheck (BasicAuthData u p) = return $ Authorized User

app :: Application
app = serveWithContext (Proxy :: Proxy ServantType) ctx server
  where
    ctx = (BasicAuthCheck myAuthCheck) :. EmptyContext

mainFn :: IO ()
mainFn = run 4000 app
