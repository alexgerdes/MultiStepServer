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

module Main where

import Data.List
import Ideas.Text.JSON
import Network.CGI
import Control.Monad.Trans.Class (lift)

import Polynomial.PolyDiagnosis (stringDiagnosis)

cgiMain :: CGI CGIResult
cgiMain = do
   setHeader "Content-type" "text/json"
   setHeader "Access-Control-Allow-Origin" "*"
   mtxt  <- getInput "input"
   -- tsks  <- lift loadTasks
   -- bgs'  <- lift $ readFile "./buggyRules/buggys"
   -- bgsE' <- lift $ readFile "./buggyRules/buggysE"
  -- let bgs  = read bgs'  :: [String]
  -- let bgsE = read bgsE' :: [String]
   case mtxt of
      Nothing  -> fail "no input"
      Just txt -> 
         case parseJSON txt of
            Left err -> fail $ "invalid json: " ++ show err
            Right json -> 
               case evalDecoderJSON decS json of
                  Left err -> fail $ "invalid json: " ++ show err
                  Right s  -> case s of 
                     "diagnosis" -> 
                         case evalDecoderJSON df json of 
                            Left err -> fail 
                               $ "invalid json: " ++ show err
                            Right new -> output $ show new
 
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

