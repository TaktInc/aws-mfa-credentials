{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module AwsMfaCredentials.Interpreters.CredentialsWriter where

import Control.Lens.Operators ((^.))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Freer (Member, Eff, handleRelay, send)
import Control.Monad.Freer.Exception (Exc, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Freer.Writer (Writer(..))
import Data.Ini (Ini(Ini), writeIniFile, readIniFile)
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import Network.AWS.Auth (credFile)
import Network.AWS.STS.Types ( Credentials
                             , cAccessKeyId
                             , cSecretAccessKey
                             , cSessionToken
                             )
import System.FileLock (withFileLock, SharedExclusive(Exclusive))
import System.FilePath (replaceBaseName)

newtype CredentialsFileParseError = CredentialsFileParseError String

-- | Write out credentials to the AWS credentials file.
--
-- This is atomic if all accesses use our locking protocol.
writeCredentials :: forall m r a proxy . ( MonadIO m
                                         , MonadCatch m
                                         , Member m r
                                         , Member (Exc CredentialsFileParseError) r
                                         )
                 => proxy m
                 -> Eff (Writer (Text, Credentials) ': r) a
                 -> Eff r a
writeCredentials _ = handleRelay pure bind
  where
    bind :: Writer (Text, Credentials) x
         -> (x -> Eff r a)
         -> Eff r a
    bind (Writer (profile, creds)) cont =
      (send @m . liftIO $ write profile creds) >>= \case
        Nothing -> cont ()
        Just e -> throwError e
    write profile creds = do
      file <- credFile
      withFileLock (replaceBaseName file ".credentials.lock") Exclusive $ \_ -> do
        readIniFile file >>= \case
          Left s -> return . Just $ CredentialsFileParseError s
          Right (Ini ini) -> do
            writeIniFile file $ Ini $ M.insert profile (toSection creds) ini
            return Nothing
    toSection creds =
      M.fromList [ ("aws_access_key_id", creds ^. cAccessKeyId)
                 , ("aws_secret_access_key", creds ^. cSecretAccessKey)
                 , ("aws_session_token", creds ^. cSessionToken)
                 ]