{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AwsMfaCredentials.Interpreters.Wait (runWait) where

import AwsMfaCredentials.Effects.Wait (Wait(..))
import Control.Concurrent (threadDelay)
import Control.Monad.Freer (Member, Eff, handleRelay, send)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroseconds = floor . (* 10 ^ (6 :: Integer))

runWait :: forall m r a proxy . (MonadIO m, Member m r)
        => proxy m
        -> Eff (Wait ': r) a
        -> Eff r a
runWait _ = handleRelay pure bind
  where
    bind :: Wait x
         -> (x -> Eff r a)
         -> Eff r a
    bind (WaitUntil time) cont = (send @m . liftIO $ waitUntil time) >>= cont
    waitUntil time = getCurrentTime >>= threadDelay . nominalDiffTimeToMicroseconds . diffUTCTime time
