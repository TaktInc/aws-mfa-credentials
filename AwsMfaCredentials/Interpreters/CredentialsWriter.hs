{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module AwsMfaCredentials.Interpreters.CredentialsWriter where

import Control.Lens.Operators
  ( (^.)
  )
import Control.Monad.Freer
  ( Eff
  , Member
  , handleRelay
  , send
  )
import Control.Monad.Freer.Exception
  ( Exc
  , throwError
  )
import Control.Monad.Freer.Writer
  ( Writer(..)
  )
import Data.Ini
  ( Ini(..)
  , readIniFile
  , writeIniFile
  )
import qualified Data.HashMap.Strict as M
import Data.Text
  ( Text
  )
import Network.AWS.Auth
  ( credFile
  )
import Network.AWS.STS.Types
  ( Credentials
  , cAccessKeyId
  , cSecretAccessKey
  , cSessionToken
  )
import System.FileLock
  ( SharedExclusive(..)
  , withFileLock
  )
import System.FilePath
  ( replaceBaseName
  )

newtype CredentialsFileParseError = CredentialsFileParseError String

-- | Write out credentials to the AWS credentials file.
--
-- This is atomic if all accesses use our locking protocol.
writeCredentials
  :: forall r a . ( Member IO r
                  , Member (Exc CredentialsFileParseError) r
                  )
  => Eff (Writer (Text, Credentials) ': r) a
  -> Eff r a
writeCredentials = handleRelay pure bind
  where
    bind :: Writer (Text, Credentials) x
         -> (x -> Eff r a)
         -> Eff r a
    bind (Writer (profile, creds)) cont =
      (send $ write profile creds) >>= \case
        Left e -> throwError e
        Right _ -> cont ()

    write profile creds = do
      file <- credFile
      let lockFile = replaceBaseName ".credentials.lock" file
      withFileLock lockFile Exclusive $ \_ -> do
        readIniFile file >>= \case
          Left s -> return . Left $ CredentialsFileParseError s
          Right (Ini ini) -> do
            writeIniFile file . Ini $ M.insert profile (toSection creds) ini
            return $ Right ()

    toSection creds =
      M.fromList [ ("aws_access_key_id", creds ^. cAccessKeyId)
                 , ("aws_secret_access_key", creds ^. cSecretAccessKey)
                 , ("aws_session_token", creds ^. cSessionToken)
                 ]
