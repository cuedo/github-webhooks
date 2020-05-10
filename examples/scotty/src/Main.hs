module Main
    ( main
    ) where

import           Control.Monad.IO.Class       ( liftIO )
import qualified Data.ByteString.Char8        as C8
import           GitHub.Data.Webhooks.Events  ( IssueCommentEvent(..) )
import           GitHub.Data.Webhooks.Payload ( HookIssueComment(..) )
import           System.Environment           ( lookupEnv )

-- Using scotty to serve the API
import           Web.Scotty


main :: IO ()
main = do
    port <- maybe 8080 read <$> lookupEnv "PORT"
    key <- maybe mempty C8.pack <$> lookupEnv "KEY"
    liftIO . putStrLn $ "Using test secret " ++ show key
    liftIO . putStrLn $ "Perhaps run 'ngrok http " ++ show port ++ "' for a forwarding address"

    scotty port $
        post "/" $ do
            -- FIXME: Secret key is not verified in scotty example
            ev <- jsonData
            liftIO . print $ (whIssueCommentBody . evIssueCommentPayload) (ev :: IssueCommentEvent)
