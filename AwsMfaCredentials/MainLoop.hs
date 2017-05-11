{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AwsMfaCredentials.MainLoop where

import AwsMfaCredentials.Effects.AWS (AWS, getSessionToken)
import AwsMfaCredentials.Effects.PasswordPrompt
  (PasswordPrompt, passwordPrompt)
import AwsMfaCredentials.Effects.Wait (waitUntil)
import Control.Lens.Operators ((&), (?~), (.~), (^.))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Writer (Writer, tell)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime)
import Network.AWS.STS.Types (Credentials, cExpiration)
import qualified Network.AWS.STS.GetSessionToken as STS
import Numeric.Natural (Natural)

-- | Command line options
data Opts = Opts
  { -- | The MFA device ID
    serialNumber :: Text
  , -- | The duration, in seconds, that the credentials should remain valid
    duration :: Maybe Natural
  , -- | The profile whose credentials we're managing
    profile :: Text
  , -- | The amount of time, in seconds, before credential expiry to refresh
    refreshLeadTime :: Natural
  }

-- | The business logic of the aws-mfa-credentials main loop
mainLoopBody :: forall r . ( Member (PasswordPrompt String Text) r
                           , Member AWS r
                           , Member (Writer (Text, Credentials)) r
                           , Member (Writer UTCTime) r
                           ) => Opts -> Eff r ()
mainLoopBody (Opts {..}) = do
    mfa <- passwordPrompt . T.unpack $ T.concat
      [ "Enter MFA token for "
      , profile
      , " from device "
      , serialNumber
      ]
    let req = STS.getSessionToken & STS.gstTokenCode ?~ mfa
                                  & STS.gstDurationSeconds .~ duration
                                  & STS.gstSerialNumber ?~ serialNumber
    creds <- getSessionToken req
    tell (mfaProfile, creds)
    waitUntil . addUTCTime refreshLeadTime' $ creds ^. cExpiration
  where
    mfaProfile = T.append profile "-mfa"

    refreshLeadTime' = 0 - fromIntegral refreshLeadTime
