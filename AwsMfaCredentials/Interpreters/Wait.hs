{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AwsMfaCredentials.Interpreters.Wait (runWait) where

import AwsMfaCredentials.Effects.Wait (Wait(..))
import Control.Concurrent (threadDelay)
import Control.Monad.Freer (Member, Eff, handleRelay, send)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroseconds = floor . (* 10 ^ (6 :: Integer))

runWait :: forall r a . (Member IO r)
        => Eff (Wait ': r) a
        -> Eff r a
runWait = handleRelay pure bind
  where
    bind :: Wait x
         -> (x -> Eff r a)
         -> Eff r a
    bind (WaitUntil time) cont = (send $ waitUntil time) >>= cont
    waitUntil time = getCurrentTime >>= threadDelay . nominalDiffTimeToMicroseconds . diffUTCTime time
