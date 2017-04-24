{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module AwsMfaCredentials.Interpreters.PasswordPrompt
  ( RunAskPassFailure(..)
  , runWithAskPass
  ) where

import AwsMfaCredentials.Effects.PasswordPrompt
  ( PasswordPrompt(..)
  )
import Control.Lens.Operators
  ( (<&>)
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
import Data.Text
  ( Text
  , strip
  )
import qualified Data.Text.IO as T
import System.Exit
  ( ExitCode(..)
  )
import System.Process
import System.Timeout
  ( timeout
  )

data RunAskPassFailure = RunAskPassTimeout
                       | RunAskPassFailure !ExitCode

runAskPassWithTimeout :: String -> IO (Either RunAskPassFailure Text)
runAskPassWithTimeout prompt =
    timeout (120 * 10 ^ (6 :: Int)) run <&> \case
      Just (Left code) -> Left $ RunAskPassFailure code
      Just (Right pass) -> Right pass
      Nothing -> Left RunAskPassTimeout
  where
    procSpec =
      (proc "ssh-askpass" [ prompt ]) { std_in = NoStream
                                      , std_out = CreatePipe
                                      }

    run = withCreateProcess procSpec $ \_ (Just out) _ p -> do
      pass <- T.hGetContents out
      waitForProcess p <&> \case
        ExitSuccess -> Right $ strip pass
        e -> Left e

-- | Run the PasswordPrompt effect using ssh-askpass.
runWithAskPass :: forall r a . ( Member IO r
                               , Member (Exc RunAskPassFailure) r
                               )
               => Eff (PasswordPrompt String Text ': r) a
               -> Eff r a
runWithAskPass = handleRelay pure bind
  where
    bind :: PasswordPrompt String Text x
         -> (x -> Eff r a)
         -> Eff r a
    bind (PasswordPrompt prompt) cont =
      (send $ runAskPassWithTimeout prompt) >>= \case
        Left err -> throwError err
        Right pass -> cont pass
