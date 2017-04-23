{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module AwsMfaCredentials.Interpreters.Wait (runWait) where

import Control.Concurrent (threadDelay)
import Control.Monad.Freer (Member, Eff, runNat)
import Control.Monad.Freer.Writer (Writer(..))
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroseconds = floor . (* 10 ^ (6 :: Integer))

runWait :: forall r a . (Member IO r)
        => Eff ((Writer UTCTime) ': r) a
        -> Eff r a
runWait = runNat waitUntil
  where
    waitUntil :: Writer UTCTime x -> IO x
    waitUntil (Writer time) = getCurrentTime >>= threadDelay . nominalDiffTimeToMicroseconds . diffUTCTime time
