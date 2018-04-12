{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module HeaderInput where

import Servant ( Header
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

type ServantType =  "name" :> Header "CustomHeader" String :> Get '[PlainText] String

server :: Server ServantType
server = handlerHeaderName

handlerHeaderName :: Maybe String -> Handler String
handlerHeaderName customHeaderValue = case customHeaderValue of
  Just name -> return name
  Nothing -> return "No header"

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app
