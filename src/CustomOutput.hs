{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HelloWorld where

import Servant ( QueryParam
               , PlainText
               , Get
               , MimeRender(..)
               , Accept(..)
               , JSON
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, ToJSON(..), (.=))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE

-- In this example we see how to output
-- data using a custom encoding.

data ANewFormat

data Payload = Payload String String -- This is our json payload that we will output from the endpoint.

instance Accept ANewFormat where  -- Accept instance is required for output encoding as well.
  contentType _ = "text/a-new-format"

instance MimeRender ANewFormat Payload where -- This is where the actual encoding happens
  mimeRender _ (Payload itemOne itemTwo) = TE.encodeUtf8 $ T.pack $ ("ANewFormat" ++ itemOne ++ "--" ++ itemTwo)

type ServantType =  "payload" :> Get '[ANewFormat] Payload

handlerPayload :: Handler Payload
handlerPayload = return $ Payload "itemOne" "itemTwo"

server :: Server ServantType
server = handlerPayload

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app
