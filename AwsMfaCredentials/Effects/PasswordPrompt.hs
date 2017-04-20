{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module AwsMfaCredentials.Effects.PasswordPrompt where

import Control.Monad.Freer (Eff, Member, send)
import Data.String (IsString)

-- | Password prompting effects
data PasswordPrompt prompt ret a where
  PasswordPrompt :: prompt -> PasswordPrompt prompt ret ret -- ^ Request a password with the given prompt

-- | Request a password with the given prompt
passwordPrompt :: Member (PasswordPrompt prompt ret) r
               => prompt
               -> Eff r ret
passwordPrompt = send . PasswordPrompt

-- | Request a password with a default prompt
password :: forall prompt ret r . ( IsString prompt
                                  , Member (PasswordPrompt prompt ret) r
                                  ) => Eff r ret
password = passwordPrompt @prompt "Enter password"
