{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module AwsMfaCredentials.Effects.Wait where

import Control.Monad.Freer (Member, Eff, send)
import Data.Time.Clock (UTCTime)

-- | Effects for waiting
data Wait a where
  WaitUntil :: UTCTime -> Wait () -- ^ Wait until approximately a given time.

-- | Wait until approximately a given time
waitUntil :: Member Wait r => UTCTime -> Eff r ()
waitUntil = send . WaitUntil
