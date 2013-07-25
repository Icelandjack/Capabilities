-- | This is a top-level module.
-- The purpose of the module is to make effectful computations more explicit.
-- 
--   A simple example of the library in use:
-- 
-- > {-# LANGUAGE FlexibleContexts    #-}
-- > 
-- > import Prelude hiding    (putStrLn, getLine)
-- > import Capabilities      (run, Restr, (:+:))
-- > import Capabilities.IO   (TTY, W, putStrLn, getLine)
-- > 
-- > printMyName :: Restr (W :+: TTY) ()
-- > printMyName = do
-- >   name <- getLine
-- >   putStrLn ("Hey my name is " ++ name ++ "!")
-- > 
-- > main :: IO ()
-- > main = run printMyName
-- 
-- where @printMyName@ can /only/ write to files and read and write to
-- handles (note that @TTY@ is a type synonym for @Stdin :+: Stdout@).
-- 
-- Defining your own restricted actions works as follows: you want to
-- outsource summing numbers from a file to an untrusted party but don't
-- want them accessing your secret files. This is accomplished by creating
-- a new data type @SecureRead a@ which checks the filepath against the
-- string \"secret\". The function @secureRead@ cannot open such a file:
-- 
-- > {-# LANGUAGE FlexibleContexts    #-}
-- > {-# LANGUAGE DeriveFunctor       #-}
-- > {-# LANGUAGE TypeOperators       #-}
-- > 
-- > import qualified Prelude as P
-- > import Data.List
-- > import Prelude hiding (readFile, putStrLn)
-- > 
-- > import Capabilities
-- > 
-- > data SecureRead a = SecureRead FilePath (Maybe String -> a) deriving Functor
-- > 
-- > instance Run SecureRead where
-- >   runAlgebra (SecureRead filepath io) 
-- >     | "secret" `isInfixOf` filepath = io Nothing
-- >     | otherwise                     = do content <- P.readFile filepath
-- >                                          io (Just content)
-- > 
-- > secureRead :: (SecureRead :<: f) => FilePath -> Restr f (Maybe String)
-- > secureRead filepath = liftRaw (SecureRead filepath Pure)
-- > 
-- > untrusted :: FilePath -> Restr (Stdout :+: SecureRead) (Maybe Int)
-- > untrusted filepath = do
-- >   outcome <- secureRead filepath
-- >   let numbers = fmap (map read . lines) outcome
-- >   case numbers of
-- >     Just ns -> putStrLn ("Contents: " ++ show ns)
-- >     Nothing -> putStrLn "Invalid filepath!"
-- >   return (fmap sum numbers)
-- 
-- with
-- 
-- >>> run (untrusted "/tmp/file.txt")
-- Contents: [10,20,30,40]
-- Just 100
-- >>> run (untrusted "/tmp/secret.txt")
-- Invalid filepath!
-- Nothing

module Capabilities (
  module Capabilities.IO,
  module Capabilities.Internals 
  )
  where

import Capabilities.IO
import Capabilities.Internals 
