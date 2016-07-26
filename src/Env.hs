{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Env ( getEnv
           , envMaybe
           , envRead
           , read

           -- * Reexport several Readers from Data.Text
           , decimal
           , signed
           , hexadecimal
           , rational
           , double

           -- * Data types
           , EnvironmentException(..)
           , Reader(..)
           ) where

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
import           Data.Text.Read
import           Data.Typeable
import           Prelude                hiding (read)
import           System.Environment     (lookupEnv)
import           Text.Read              (readEither)

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
getEnv :: (MonadThrow m, MonadIO m) => Text -> m Text
getEnv key =
  envMaybe key >>=
  \case
     Nothing -> throwM $ EnvVarNotFound key
     Just v -> return v

-- | Get value of environment variable.
envMaybe :: (MonadIO m) => Text -> m (Maybe Text)
envMaybe key = liftIO $ liftM (pack <$>) (lookupEnv (unpack key))

-- | Get value of environment variable and parse it using specific reader.
envRead :: (MonadIO m) => Reader a -> Text -> m (Maybe a)
envRead r = fmap (join . fmap ((fmap fst . fromRight) . r)) . envMaybe

-- | Generic reader for readable values.
--
-- Keep in mind that it's always better from performance view to use specific
-- Reader functions like @'decimal'@ instead of this generic one.
read :: Read a => Reader a
read = fmap (\v -> (v, "")) . readEither . unpack

fromRight :: Either a b -> Maybe b
fromRight = either (const Nothing) Just
