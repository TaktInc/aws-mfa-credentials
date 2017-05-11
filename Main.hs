{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Main where

import AwsMfaCredentials.MainLoop (Opts(..), mainLoopBody)
import qualified AwsMfaCredentials.Effects.AWS as E
import AwsMfaCredentials.Effects.PasswordPrompt (PasswordPrompt)
import AwsMfaCredentials.Interpreters.AWS
  (AWSResponseFailure(..), runInAWSMonad)
import AwsMfaCredentials.Interpreters.CredentialsWriter
  (CredentialsFileParseError(..), writeCredentials)
import AwsMfaCredentials.Interpreters.PasswordPrompt
  (RunAskPassFailure(..), runWithAskPass)
import AwsMfaCredentials.Interpreters.Wait (runWait)
import Control.Monad.Freer (Eff, Member, runM, runNat, send)
import Control.Monad.Freer.Exception (Exc, runError)
import Control.Monad.Freer.Writer (Writer)
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Network.AWS (AWS, newEnv, runAWS, Credentials(..), runResourceT)
import Network.AWS.Auth (credFile)
import qualified Network.AWS.STS.Types as STS
import Options.Applicative
import Options.Applicative.Text (textOption)
import System.IO (hPutStrLn, stderr)

-- | Parser for command line options
optsParser :: Parser Opts
optsParser =  Opts
          <$> textOption
                ( long "mfa-serial-number"
               <> metavar "SERIAL_NUMBER"
               <> help "The ID of the MFA device to authenticate with"
                )
          <*> (optional $ option auto
                ( long "duration"
               <> metavar "DURATION_SECONDS"
               <> help "The lifetime of the credentials (uses AWS default if unset)"
                )
              )
          <*> textOption
                ( long "profile"
               <> metavar "PROFILE"
               <> help "The name of the profile whose credentials we're managing"
               <> value "default"
               <> showDefault
                )
          <*> option auto
                ( long "refresh-lead-time"
               <> metavar "LEAD_SECONDS"
               <> help "The amount of time before credential expiry to refresh the credentials"
               <> value 300
               <> showDefault
                )

-- | Full command line parser with usage string.
optsParserInfo :: ParserInfo Opts
optsParserInfo = info (optsParser <**> helper)
  ( fullDesc
 <> progDesc "Keep your AWS credentials file up to date with MFA-carrying temporary credentials"
  )

handleError :: forall r a . (Member IO r)
            => (a -> String)
            -> Eff (Exc a ': r) Bool
            -> Eff r Bool
handleError showE e = runError e >>= \case
    Left err -> do
      send . hPutStrLn stderr $ showE err
      return True
    Right b -> return b

mainLoop :: Opts -> AWS ()
mainLoop opts = do
    (interpret $ mainLoopBody opts) >>= \case
      False -> mainLoop opts
      True -> return ()
  where
    interpret :: Eff '[ Writer UTCTime
                      , Writer (Text, STS.Credentials)
                      , E.AWS
                      , PasswordPrompt String Text
                      , Exc CredentialsFileParseError
                      , Exc AWSResponseFailure
                      , Exc RunAskPassFailure
                      , IO
                      , AWS
                      ] () -> AWS Bool
    interpret =
      runM
      . runNat @AWS liftIO
      . handleError runAskPassFailure
      . handleError awsResponseFailure
      . handleError credentialsFileParseError
      . (False <$)
      . runWithAskPass
      . runInAWSMonad
      . writeCredentials
      . runWait

    runAskPassFailure RunAskPassTimeout =
      "Timed out waiting for MFA token"
    runAskPassFailure (RunAskPassFailure _) =
      "User cancelled token input"

    awsResponseFailure (AWSResponseFailure i) =
      "Requesting temporary credentials from AWS failed with HTTP error code " ++ show i

    credentialsFileParseError (CredentialsFileParseError msg) =
      "Error parsing the AWS credentials file: " ++ msg

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  env <- credFile >>= newEnv . FromFile (profile opts)
  runResourceT . runAWS env $ mainLoop opts
