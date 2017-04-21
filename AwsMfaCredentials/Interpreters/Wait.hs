{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AwsMfaCredentials.Interpreters.Wait (runWait) where

import Control.Concurrent (threadDelay)
import Control.Monad.Freer (Member, Eff, handleRelay, send)
import Control.Monad.Freer.Writer (Writer(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroseconds = floor . (* 10 ^ (6 :: Integer))

runWait :: forall m r a proxy . (MonadIO m, Member m r)
        => proxy m
        -> Eff ((Writer UTCTime) ': r) a
        -> Eff r a
runWait _ = handleRelay pure bind
  where
    bind :: Writer UTCTime x
         -> (x -> Eff r a)
         -> Eff r a
    bind (Writer time) cont = (send @m . liftIO $ waitUntil time) >>= cont
    waitUntil time = getCurrentTime >>= threadDelay . nominalDiffTimeToMicroseconds . diffUTCTime time
