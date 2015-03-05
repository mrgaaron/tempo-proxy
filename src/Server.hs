{-# LANGUAGE OverloadedStrings #-}
module Network.TempoDB.Proxy where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Monoid (mappend, mempty)
import Control.Monad(forM_, join, liftM)
import Control.Monad.IO.Class (liftIO)
import Snap.Http.Server
import Snap.Core
import qualified Blaze.ByteString.Builder as Builder
import qualified Network.Http.Client as Client
import Network.Http.Types (retrieveHeaders)
import qualified System.IO.Streams as Streams
import Data.CaseInsensitive (mk, original)

conf = (setPort 8080) . (setBind $ "127.0.0.1") $ emptyConfig

swapHeaders :: Client.Response -> Response -> Response
swapHeaders resp1 resp2 = let cleanResp = foldl (\r h -> deleteHeader (fst h) r) resp2 $ listHeaders resp2 
                              newHdrs = retrieveHeaders $ Client.getHeaders resp1 in 
  foldl xferHeader cleanResp newHdrs where
    xferHeader r (k, v) = addHeader (mk k) v r

streamHandler resp i1 = do 
  i2 <- Streams.map Builder.fromByteString i1
  x <- Streams.fold mappend mempty i2
  return $ (resp, Builder.toByteString x)

getBody :: MonadSnap m => m L.ByteString
getBody = readRequestBody (10 * 1024)

castSnapMethod :: Method -> Client.Method
castSnapMethod GET = Client.GET
castSnapMethod POST = Client.POST
castSnapMethod PUT = Client.PUT
castSnapMethod DELETE = Client.DELETE

proxy :: Request -> Method -> L.ByteString -> IO (Client.Response, S.ByteString)
proxy req method body = do 
  let path = rqURI req
  c <- Client.openConnection "api.tempo-db.com" 443
  q <- Client.buildRequest $ do
    Client.http (castSnapMethod method) path
    forM_ (listHeaders req) $ \(k,v) -> do
      Client.setHeader (original k) v
  Client.sendRequest c q $ \o ->
    Streams.write (Just (Builder.fromLazyByteString body)) o 
  (resp, body) <- Client.receiveResponse c streamHandler
  Client.closeConnection c
  return (resp, body)

handler :: Snap ()
handler = do
  req <- getRequest
  reqBody <- getBody
  let method = rqMethod req
  (resp, body) <- liftIO $ proxy req method reqBody
  modifyResponse $ setResponseCode $ Client.getStatusCode resp
  r <- getResponse
  let proxied = swapHeaders resp r
  modifyResponse $ \_ -> proxied
  writeBS $ body

server = httpServe conf handler

