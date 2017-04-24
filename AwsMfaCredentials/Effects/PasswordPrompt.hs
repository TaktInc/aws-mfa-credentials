{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
module AwsMfaCredentials.Effects.PasswordPrompt where

import Control.Monad.Freer
  ( Eff
  , Member
  , send
  )

-- | Password prompting effects
data PasswordPrompt prompt ret a where
  -- | Request a password with the given prompt
  PasswordPrompt :: prompt
                 -> PasswordPrompt prompt ret ret

-- | Request a password with the given prompt
passwordPrompt :: Member (PasswordPrompt prompt ret) r
               => prompt
               -> Eff r ret
passwordPrompt = send . PasswordPrompt
