module Main where

import Control.Monad (msum)
import Control.Monad.Trans.Class
import Language.Haskell.Interpreter
import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, path, dir)
import Happstack.Server.Internal.Monads
import Text.JSON.Generic
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as C
import Text.Regex.Posix

main :: IO ()
main = simpleHTTP nullConf $
	msum [dir "eval" $ path $ \s -> handleRequest s]

data Response = Response
	{ success :: Bool
	, cacheHit :: Bool
	, result :: String
	, error :: String
	} deriving (Show, Data, Typeable)

pattern :: String
pattern = "^([0-9+-/*().^]|sin|cos|tan|asin|acos|atan|sqrt)*$"

myConnectionInfo :: R.ConnectInfo
myConnectionInfo = R.defaultConnectInfo

handleRequest :: String ->  ServerPartT IO String
handleRequest exp = lift $ evalExp exp

evalExp :: String -> IO String
evalExp exp = do
    cachedResult <- checkCache exp
    x <- returnResult cachedResult exp
    return $ encodeJSON x

returnResult :: String -> String -> IO Response
returnResult [] exp = case (exp =~ pattern) of True -> do
                                                        a <- runInterpreter $ setImports ["Prelude"] >> eval exp
                                                        let (error, success, result) = case a of Left _ -> ("Syntax Error", False, "")
		                                                                                 Right b -> ("", True, b)
                                                        addToCache success exp result
                                                        return $ Response success False result error
                                               False -> return $ Response False False "" "Insanitary Input"
returnResult x _ = return $ Response True True x ""

addToCache :: Bool -> String -> String -> IO (Either R.Reply R.Status)
addToCache True exp res = do
                    conn <- R.connect myConnectionInfo
                    R.runRedis conn $ R.set (C.pack exp) (C.pack res)
addToCache False _ _ = return $ Right R.Ok

checkCache :: String -> IO String
checkCache exp = do
        x <- checkCacheAux exp
        return $ case x of Left _ -> ""
                           Right (Just a) -> C.unpack a
                           Right (Nothing) -> ""

checkCacheAux :: String -> IO (Either R.Reply (Maybe C.ByteString))
checkCacheAux exp = do
	conn <- R.connect myConnectionInfo
	R.runRedis conn $ R.get $ C.pack exp

