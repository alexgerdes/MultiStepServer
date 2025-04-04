--   request:
--   { "service" : diagnosis
--   , "step" : { "x^2", "sqrt(x)"}
--   }
--    
--   response:
--   
--   { "feedback": "Let op: invert slope"
--   , "equiv" : true
--   }
--
-------------------------------------------------------------------


-- http://localhost/mbt-server.cgi?input={"task":{"x1":1,"x2":2,"x3":5,"y1":8,"y2":10},"answer":42}
-- https://ideastest.science.uu.nl/cgi-bin/mbt-server.cgi?input={"task":{"x1":1,"x2":2,"x3":5,"y1":8,"y2":10},"answer":42}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Database.SQLite3 (withDatabase, exec)
import qualified Data.Text as T
import Ideas.Text.JSON
import Network.CGI
import Control.Monad.Trans.Class (lift)

import Polynomial.PolyDiagnosis (stringDiagnosis)

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "text/json"
  setHeader "Access-Control-Allow-Origin" "*"
  txt     <- maybe (fail "no input") id <$> getInput "input"
  liftIO (logDB txt)
  json    <- liftCGI (parseJSON txt)
  service <- liftCGI (evalDecoderJSON decS json)
  case service of 
    "diagnosis" -> liftCGI (evalDecoderJSON df json) >>= output . show
    _           -> fail "unknown service"

liftCGI :: Show err => Either err a -> CGI a
liftCGI = either (fail . show) return

logDB :: String -> IO ()
logDB txt = withDatabase "/workspace/mbt.db" (flip exec query)
 where
  query = T.concat ["INSERT INTO requests (req) VALUES ('", T.pack txt, "');"]

decS :: DecoderJSON String
decS = jObject $ jKey "service" jString

decT :: DecoderJSON (String, String)
decT = jObject 
   $ jKey "step" 
   $ jArray2 (\a -> \b -> (a, b)) 
   jString jString

responseT :: (String, Integer) -> JSON
responseT (s, n) = 
   Object [("feedback", String s), 
          ("equiv", Integer n)]

df :: DecoderJSON JSON
df = (responseT . stringDiagnosis) <$> decT  

main :: IO ()
main = runCGI (handleErrors cgiMain)

