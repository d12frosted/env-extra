{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Env ( EnvironmentException(..)
           , env
           , envMaybe
           , envDecimal
           , envSignedDecimal
           , envHexadecimal
           , envSignedHexademical
           , envRational
           , envDouble
           , envRead
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

-- | Get value of environment variable and parse it as integral number. This
-- function does not handle leading sign character. Use @'envSignedDecimal'@
-- instead.
envDecimal :: (MonadIO m, Integral a) => Text -> m (Maybe a)
envDecimal = envRead decimal

-- | Get value of environment variable and parse it as integral number. This
-- function handles leading sign character, but works slower than
-- @'envDecimal'@.
envSignedDecimal :: (MonadIO m, Integral a) => Text -> m (Maybe a)
envSignedDecimal = envRead (signed decimal)

-- | Get value of environment variable and parse it as hexadecimal integral
-- number, consisting of an optional leading "0x". This function does not handle
-- leading sign character. Use @'envSignedHexademical'@ instead.
envHexadecimal :: (MonadIO m, Integral a) => Text -> m (Maybe a)
envHexadecimal = envRead hexadecimal

-- | Get value of environment variable and parse it as hexadecimal integral
-- number, consisting of an optional leading "0x". This function handles leading
-- sign character, but works slower than @'envHexadecimal'@.
envSignedHexademical :: (MonadIO m, Integral a) => Text -> m (Maybe a)
envSignedHexademical = envRead (signed hexadecimal)

-- | Get value of environment variable and parse it as fractional number.
envRational :: (MonadIO m, Fractional a) => Text -> m (Maybe a)
envRational = envRead rational

-- | Get value of environment variable and parse it as double. This function is
-- almost ten times faster than @'envRational'@, but is slightly less accurate.
envDouble :: (MonadIO m) => Text -> m (Maybe Double)
envDouble = envRead double

-- | Get value of environment variable and parse it using specific reader.
envRead :: (MonadIO m) => Reader a -> Text -> m (Maybe a)
envRead r = fmap (join . fmap ((fmap fst . fromRight) . r)) . envMaybe

fromRight :: Either a b -> Maybe b
fromRight = either (const Nothing) Just
