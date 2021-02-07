{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module MultipartData where

import Servant ( PlainText
               , Post
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Server, Application, serve)
import Network.Wai.Handler.Warp (run)

import Servant.Multipart  -- This is the module that contain the multipart handling stuff. It is part of a separate package called 'servant-multipart'

uploadHandlerMem :: MultipartData Mem -> Handler String -- Handlers that accept multipart data have either 'MultipartData Mem' or 'MultipartData Tmp' as it's argument.
uploadHandlerMem mData = do                             -- If the upload has to be stored in memory then the type should be Multipart Mem. If the files have to be stored 
  let output = (inputs mData, files mData)              -- in a tmp folder, the type needs to be 'MultipartData Tmp'
  return $ show output                                  -- Then we use the 'inputs' and 'files' functions from the Servant.Multipart module to access the input fields and upload files

uploadHandlerFile :: MultipartData Tmp -> Handler String  -- An upload handler that stores the uploaded file in a tmp location. See the sample usage.
uploadHandlerFile mData = do
  let output = (inputs mData, files mData)
  return $ show output

type ServantType = "uploadToTmpFile" :>  MultipartForm Tmp (MultipartData Tmp) :> Post '[PlainText] String
              :<|> "uploadToMem" :> MultipartForm Mem (MultipartData Mem) :> Post '[PlainText] String

server :: Server ServantType
server = uploadHandlerFile :<|> uploadHandlerMem

app :: Application
app = serve (Proxy :: Proxy ServantType) server

mainFn :: IO ()
mainFn = run 4000 app

-- $ curl -v -F name=sras -F age=35 -F upload=@/tmp/upload  http://127.0.0.1/uploadToMem
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 80 (#0)
-- > POST /uploadToMem HTTP/1.1
-- > Host: 127.0.0.1
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- > Content-Length: 444
-- > Expect: 100-continue
-- > Content-Type: multipart/form-data; boundary=------------------------9d37ec5069a8d9ec
-- >
-- < HTTP/1.1 100 Continue
-- < HTTP/1.1 200 OK
-- < Server: nginx/1.10.3 (Ubuntu)
-- < Date: Sun, 22 Jul 2018 13:41:37 GMT
-- < Content-Type: text/plain;charset=utf-8
-- < Transfer-Encoding: chunked
-- < Connection: keep-alive
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- ([Input {iName = "name", iValue = "sras"},Input {iName = "age", iValue = "35"}],[FileData {fdInputName = "upload", fdFileName = "upload", fdFileCType = "app
-- lication/octet-stream", fdPayload = "This is some random\\nContent in the uploaded\\n file.\n"}])
--
-- $ curl -v -F name=sras -F age=35 -F upload=@/tmp/upload  http://127.0.0.1/uploadToTmpFile
-- *   Trying 127.0.0.1...
-- * Connected to 127.0.0.1 (127.0.0.1) port 80 (#0)
-- > POST /uploadToTmpFile HTTP/1.1
-- > Host: 127.0.0.1
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- > Content-Length: 444
-- > Expect: 100-continue
-- > Content-Type: multipart/form-data; boundary=------------------------d20f52c9ba99c75d
-- >
-- < HTTP/1.1 100 Continue
-- < HTTP/1.1 200 OK
-- < Server: nginx/1.10.3 (Ubuntu)
-- < Date: Sun, 22 Jul 2018 13:41:44 GMT
-- < Content-Type: text/plain;charset=utf-8
-- < Transfer-Encoding: chunked
-- < Connection: keep-alive
-- <
-- * Connection #0 to host 127.0.0.1 left intact
-- ([Input {iName = "name", iValue = "sras"},Input {iName = "age", iValue = "35"}],[FileData {fdInputName = "upload", fdFileName = "upload", fdFileCType = "app
-- lication/octet-stream", fdPayload = "/tmp/servant-multipart27893-0.buf"}])sras@servant-examples :
