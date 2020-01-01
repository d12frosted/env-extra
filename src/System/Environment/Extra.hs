{-|
Module      : System.Environment.Extra
Description : Safe helpers for accessing and modifying environment variables
Copyright   : (c) Boris Buliga, 2016-2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX

The module defines function 'setEnv' - a lifted version of 'E.setEnv' that works
with 'Text' input and various safe versions of 'E.lookupEnv' that allow one to
get any 'IsString' ('getEnv' and 'envMaybe') or even provide a 'Reader' to parse
the value ('envRead')

>>> getEnv "HOME"
"/Users/d12frosted"
>>> getEnv "WHAAT"
*** Exception: Could not find value of $WHAAT in environment.
>>> setEnv "WHAAT" "HOME"
>>> getEnv "WHAAT"
"HOME"
>>> getEnv "WHAAT" >>= getEnv
"/Users/d12frosted"
>>> getEnv "WHAAT" >>= putStrLn
HOME
>>> setEnv "AGE" "12"
>>> envMaybe "AGE"
Just "12"
>>> envRead decimal "AGE"
Just 12
-}

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

--------------------------------------------------------------------------------

module System.Environment.Extra
  ( setEnv
  , getEnv
  , envMaybe
  , envRead
  , read

    -- * Data types
  , EnvironmentException(..)

    -- * Reexport several Readers from Data.Text
  , Reader
  , decimal
  , signed
  , hexadecimal
  , rational
  , double
  ) where

--------------------------------------------------------------------------------

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.String            (IsString, fromString)
import           Data.Text
import           Data.Text.Read
import           Data.Typeable
import           Prelude                hiding (read)
import qualified System.Environment     as E (lookupEnv, setEnv)
import           Text.Read              (readEither)

--------------------------------------------------------------------------------
-- EnvironmentException definition

-- | Exceptions that can occur during reading the environment variable.
newtype EnvironmentException =
  EnvVarNotFound Text
  deriving (Typeable)

instance Exception EnvironmentException

instance Show EnvironmentException where
  show (EnvVarNotFound var) = "Could not find value of $" ++ unpack var ++ " in environment."

--------------------------------------------------------------------------------
-- Setting value

-- | Set value of environment variable.
--
-- Thorws 'IOException'.
--
-- >>> envMaybe "NAME"
-- Nothing
-- >>> setEnv "NAME" "Boris"
-- >>> envMaybe "NAME"
-- Just "Boris"
setEnv :: (MonadThrow m, MonadIO m) => Text -> Text -> m ()
setEnv k v = liftIO $ E.setEnv (unpack k) (unpack v)

--------------------------------------------------------------------------------
-- Getting value

-- | Get value of environment variable.
--
-- Throws 'EnvVarNotFound'.
--
-- >>> getEnv "NAME"
-- *** Exception: Could not find value of $NAME in environment.
-- >>> getEnv "HOME"
-- "/Users/d12frosted"
getEnv :: ( MonadThrow m, MonadIO m, IsString a ) => Text -> m a
getEnv key =
  envMaybe key >>=
  \case
     Nothing -> throwM $ EnvVarNotFound key
     Just v -> return v

-- | Get value of environment variable.
-- >>> getEnv "NAME"
-- Nothing
-- >>> getEnv "HOME"
-- Just "/Users/d12frosted"
envMaybe :: ( MonadIO m, IsString a ) => Text -> m (Maybe a)
envMaybe key = liftIO $ fmap (fromString <$>) (E.lookupEnv (unpack key))

-- | Get value of environment variable and parse it using specific reader.
-- >>> setEnv "AGE" "12"
-- >>> envMaybe "AGE"
-- Just "12"
-- >>> envRead decimal "AGE"
-- Just 12
envRead :: (MonadIO m) => Reader a -> Text -> m (Maybe a)
envRead r = fmap (((fmap fst . fromRight) . r) =<<) . envMaybe

-- | Generic reader for readable values.
--
-- Keep in mind that it's always better from performance view to use specific
-- Reader functions like @'decimal'@ instead of this generic one.
read :: Read a => Reader a
read = fmap (, "") . readEither . unpack

fromRight :: Either a b -> Maybe b
fromRight = either (const Nothing) Just

--------------------------------------------------------------------------------
