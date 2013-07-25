{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}

module System.Environment.Capabilities (
  -- * The environment type
  Env,
  getEnv,
  getArgs,
  getEnvironment,
  getProgName
#if MIN_VERSION_base(4,6,0)
  getExecutablePath,
  lookupEnv
#endif
  ) where

import qualified System.Environment as Env

import Capabilities.Internals
import qualified Capabilities.IO as IO

data Env a
  = GetEnv String (String -> a)
  | GetEnvironment ([(String, String)] -> a)
  | GetArgs ([String] -> a)
  | GetProgName (String -> a)
  deriving Functor
           
instance Run Env where
  runAlgebra (GetArgs        io) = Env.getArgs        >>= io
  runAlgebra (GetProgName    io) = Env.getProgName    >>= io
  runAlgebra (GetEnv key     io) = Env.getEnv key     >>= io
  runAlgebra (GetEnvironment io) = Env.getEnvironment >>= io

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (not including the program name).
getArgs :: (Env :<: f) => Restr f [String]
getArgs = liftRaw (GetArgs Pure)

{-|
Computation 'getProgName' returns the name of the program as it was
invoked.

However, this is hard-to-impossible to implement on some non-Unix
OSes, so instead, for maximum portability, we just return the leafname
of the program as invoked. Even then there are some differences
between platforms: on Windows, for example, a program invoked as foo
is probably really @FOO.EXE@, and that is what 'getProgName' will return.
-}
getProgName :: (Env :<: f) => Restr f String
getProgName = liftRaw (GetProgName Pure)

-- | Computation 'getEnv' @var@ returns the value
-- of the environment variable @var@.
--
-- This computation may fail with:
--
--  * 'System.IO.Error.isDoesNotExistError' if the environment variable
--    does not exist.
getEnv :: (Env :<: f) => String -> Restr f String
getEnv key = liftRaw (GetEnv key Pure)

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.
--
-- If an environment entry does not contain an @\'=\'@ character,
-- the @key@ is the whole entry and the @value@ is the empty string.
getEnvironment :: (Env :<: f) => Restr f [(String, String)]
getEnvironment = liftRaw (GetEnvironment Pure)

