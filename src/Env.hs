{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Env where

--------------------------------------------------------------------------------
-- * Internal imports

--------------------------------------------------------------------------------
-- * External imports

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Text
import           Data.Typeable
import           Prelude
import           System.Environment     (lookupEnv)

--------------------------------------------------------------------------------
-- * EnvironmentException definition

data EnvironmentException =
  EnvVarNotFound Text
  deriving (Typeable)

instance Exception EnvironmentException

instance Show EnvironmentException where
  show (EnvVarNotFound var) = "Could not find value of $" ++ unpack var ++ " in environment."

--------------------------------------------------------------------------------
-- * Environment lookup functions

-- | Get value of environment variable.
--
-- Throws EnvVariableNotFoundException.
env :: (MonadThrow m, MonadIO m) => Text -> m Text
env key =
  envMaybe key >>=
  \case
     Nothing -> throwM $ EnvVarNotFound key
     Just v -> return v

-- | Get value of environment variable.
envMaybe :: (MonadIO m) => Text -> m (Maybe Text)
envMaybe key = liftIO $ liftM (pack <$>) (lookupEnv (unpack key))
