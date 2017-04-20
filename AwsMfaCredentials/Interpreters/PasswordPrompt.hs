{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module AwsMfaCredentials.Interpreters.PasswordPrompt (runWithAskPass, RunAskPassFailure(..)) where

import AwsMfaCredentials.Effects.PasswordPrompt (PasswordPrompt(..))
import Control.Lens.Operators ((<&>))
import Control.Monad.Freer (Member, Eff, handleRelay, send)
import Control.Monad.Freer.Exception (Exc, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, strip)
import Data.Text.IO as T
import System.Exit (ExitCode(..))
import System.Process
import System.Timeout (timeout)

data RunAskPassFailure = RunAskPassTimeout
                       | RunAskPassFailure !ExitCode

runAskPassWithTimeout :: String -> IO (Either RunAskPassFailure Text)
runAskPassWithTimeout prompt = timeout (120 * 10 ^ (6 :: Int)) run <&> \case
    Just (Left code) -> Left $ RunAskPassFailure code
    Just (Right pass) -> Right pass
    Nothing -> Left RunAskPassTimeout
  where
    procSpec =
      (proc "ssh-askpass" [ prompt ]) { std_in = NoStream, std_out = CreatePipe }
    run = withCreateProcess procSpec $ \_ (Just out) _ p -> do
      pass <- T.hGetContents out
      waitForProcess p <&> \case
        ExitSuccess -> Right $ strip pass
        e -> Left e

-- | Run the PasswordPrompt effect using ssh-askpass.
runWithAskPass :: forall r a m proxy . ( MonadIO m
                                       , Member m r
                                       , Member (Exc RunAskPassFailure) r
                                       )
               => proxy m
               -> Eff (PasswordPrompt String Text ': r) a
               -> Eff r a
runWithAskPass _ = handleRelay pure bind
  where
    bind :: PasswordPrompt String Text x
         -> (x -> Eff r a)
         -> Eff r a
    bind (PasswordPrompt prompt) cont =
      (send @m . liftIO $ runAskPassWithTimeout prompt) >>= \case
        Left err -> throwError err
        Right pass -> cont pass
