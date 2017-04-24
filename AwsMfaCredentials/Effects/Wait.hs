{-# LANGUAGE FlexibleContexts #-}
module AwsMfaCredentials.Effects.Wait where

import Control.Monad.Freer
  ( Eff
  , Member
  , send
  )
import Control.Monad.Freer.Writer
  ( Writer(..)
  )
import Data.Time.Clock
  ( UTCTime
  )

-- | Wait until approximately a given time
waitUntil :: Member (Writer UTCTime) r => UTCTime -> Eff r ()
waitUntil = send . Writer
