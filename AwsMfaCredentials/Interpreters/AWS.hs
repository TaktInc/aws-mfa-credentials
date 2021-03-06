{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module AwsMfaCredentials.Interpreters.AWS where

import AwsMfaCredentials.Effects.AWS (AWS(..))
import Control.Lens.Operators ((^.))
import Control.Monad.Freer (Eff, Member, handleRelay, send)
import Control.Monad.Freer.Exception (Exc, throwError)
import qualified Network.AWS as Amazonka
import Network.AWS.STS.GetSessionToken
  (gstrsCredentials, gstrsResponseStatus)

newtype AWSResponseFailure = AWSResponseFailure Int

-- | Run an AWS effect in the Amazonka AWS monad.
runInAWSMonad :: forall r a . ( Member Amazonka.AWS r
                              , Member (Exc AWSResponseFailure) r
                              )
              => Eff (AWS ': r) a -> Eff r a
runInAWSMonad = handleRelay pure bind
  where
    bind :: AWS x
         -> (x -> Eff r a)
         -> Eff r a
    bind (GetSessionToken req) cont = do
      res <- send @Amazonka.AWS . Amazonka.send $ req
      case res ^. gstrsCredentials of
        Just creds -> cont creds
        Nothing ->
          throwError . AWSResponseFailure $ res ^. gstrsResponseStatus
