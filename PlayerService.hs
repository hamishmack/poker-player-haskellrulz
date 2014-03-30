module Main where

import Data.Endian
import Data.Text (pack, unpack, split)
import Network.HTTP.Server
import Network.Socket (PortNumber(PortNum))
import Network.URI (unEscapeString)
import System.Environment (getArgs)

import Player


defaultPort :: PortNumber
defaultPort = 1900

main:: IO ()
main = do
    args <- getArgs
    let port = case args of
            (x:_) -> PortNum $ toBigEndian $ read x
            _     -> defaultPort
    putStrLn $ "Listening on port " ++ show port ++ "..."
    serve port

serve:: PortNumber -> IO ()
serve port = do
    let config = defaultConfig { srvPort = port }
    serverWith config $ \_ _ request -> do
        let body = rqBody request
        responseBody <- if null body
            then return ""
            else do
                let params      = (map extractParam . split (=='&') . pack) body
                    action      = lookupWithDefault "version" "action" params
                    requestJSON = lookupWithDefault "" "game_state" params

                return $ case action of
                        "version"     -> version
                        "bet_request" -> betRequest requestJSON
                        "showdown"    -> showdown requestJSON
                        _             -> ""

        let response = respond OK :: Response String
        return $ response
            { rspBody    = responseBody
            , rspHeaders =
                [ mkHeader HdrServer "Haskell Lean Poker Player"
                , mkHeader HdrContentLength (show $ length responseBody)
                ]
            }
    where
        extractParam param =
            let [name, value] = split (=='=') param
            in (unpack name, unEscapeString $ unpack value)

        lookupWithDefault value key map = maybe value id $ lookup key map
