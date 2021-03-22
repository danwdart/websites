{- TODO: put this in a main file -}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import           Configuration.Dotenv
import           Control.Lens
import           Data.Maybe
import           Data.Text                                          as T
import           Data.Text.IO                                       as T
import           GHC.TypeLits
import           Network.Google
import           Network.Google.Auth
import           Network.Google.Resource.YouTube.Subscriptions.List
import           Network.Google.YouTube
import           System.Environment
import           System.Exit                                        (exitFailure)
import           System.IO
import           System.Info                                        (os)
import           System.Process                                     (rawSystem)

redirectPrompt ∷ AllowScopes (s ∷ [Symbol]) ⇒ OAuthClient → proxy s → IO (OAuthCode s)
redirectPrompt c p = do
  let url = formURL c p
  T.putStrLn $ "Opening URL " `T.append` url
  _ <- case os of
    "darwin" -> rawSystem "open"     [unpack url]
    "linux"  -> rawSystem "xdg-open" [unpack url]
    _        -> T.putStrLn "Unsupported OS" >> exitFailure
  T.putStrLn "Please input the authorisation code: "
  OAuthCode <$> T.getLine

main ∷ IO ()
main = do
    _ <- loadFile defaultConfig
    lgr <- newLogger Trace stdout
    oauthClient <- OAuthClient <$>
        (ClientId . T.pack <$> getEnv "GOOGLE_CLIENT_ID" ) <*>
        (GSecret . T.pack <$> getEnv "GOOGLE_CLIENT_SECRET")

    oauthCode <- redirectPrompt oauthClient youTubeReadOnlyScope
    let creds = FromClient oauthClient oauthCode

    mgr <- newManager tlsManagerSettings
    env <- newEnvWith creds lgr mgr

    let subsList = subscriptionsList "snippet" &
            subMine ?~ True &
            subMaxResults .~ 600

    ret <- runResourceT . runGoogle env $ send subsList

    let names = catMaybes (view ssTitle <$> catMaybes (view ssSnippet <$> ret ^. subItems))

    print names
