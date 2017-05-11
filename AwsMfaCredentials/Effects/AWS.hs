{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module AwsMfaCredentials.Effects.AWS where

import Control.Monad.Freer (Eff, Member, send)
import Network.AWS.STS.GetSessionToken (GetSessionToken)
import Network.AWS.STS.Types (Credentials)

-- | AWS-calling effects
data AWS a where
  -- | Get a temporary session token
  GetSessionToken :: GetSessionToken
                  -> AWS Credentials

-- | Get a temporary session token
getSessionToken :: Member AWS r
                => GetSessionToken
                -> Eff r Credentials
getSessionToken = send . GetSessionToken
