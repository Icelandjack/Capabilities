{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Capabilities.IO (
  -- * Teletype terminal
  TTY,
  
  -- ** Reading from handle
  Stdin, hGetChar, getChar, getLine, hGetContents, getContents,
  
  -- ** Write to handle
  Stdout, hPutChar, putChar, putStr, putStrLn, print,
  
  -- ** Reading and writing
  interact,

  -- * Opening and closing files
  RW,
  
  -- ** Writing to files
  W, writeFile, appendFile,
  
  -- ** Reading from files
  R, readFile,

  -- * Information from files
  Stat, stat
     
  ) where


import Capabilities.Internals

import qualified Prelude as P            (getChar, putChar, readFile,
                                          writeFile, appendFile)
import Prelude hiding                    (putChar, getChar, putStrLn, putStr,
                                          getLine, readFile, print, writeFile,
                                          appendFile, catch, getContents,
                                          interact)

import Control.Exception                 (catch, SomeException)

import System.Posix.Files                (FileStatus, getFileStatus)
import qualified System.IO as IO         (hGetChar, hPutChar, hGetContents,
                                          stdin, stdout)
import qualified System.Directory as Dir (getDirectoryContents,
                                          removeFile, removeDirectory,
                                          removeDirectoryRecursive)

import GHC.IO.Handle.Types               (Handle)

-- | Reading characters from a handle.
data Stdin a
  = HGetChar Handle (Char -> a)
  | HGetContents Handle (String -> a)
  deriving Functor

-- | Writing characters to a handle.
data Stdout a
  = HPutChar Handle Char a
  deriving Functor

-- | Allows reading and writing to handle.
type TTY = Stdin :+: Stdout

instance Run Stdin where
  runAlgebra (HGetChar     handle io) = IO.hGetChar     handle >>= io
  runAlgebra (HGetContents handle io) = IO.hGetContents handle >>= io

instance Run Stdout where
  runAlgebra (HPutChar handle c io) = IO.hPutChar handle c >>  io

-- | Writes a character to a handle.
hPutChar :: (Stdout :<: f) => Handle -> Char -> Restr f ()
hPutChar handle c = liftRaw (HPutChar handle c (Pure ()))

-- | Write a character to the standard output device
-- (same as 'hPutChar' 'stdout').
putChar :: (Stdout :<: f) => Char -> Restr f ()
putChar = hPutChar IO.stdout

-- | Write a string to the standard output device
-- (same as 'hPutStr' 'stdout').
putStr :: (Functor f, Stdout :<: f) => String -> Restr f ()
putStr = mapM_ putChar

-- | The same as 'putStr', but adds a newline character.
putStrLn :: (Functor f, Stdout :<: f) => String -> Restr f ()
putStrLn string = putStr string >> putChar '\n'

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
print :: (Show s, Functor f, Stdout :<: f) => s -> Restr f ()
print string = putStrLn (show string)

-- | Read a character from a handle.
hGetChar :: (Stdin :<: f) => Handle -> Restr f Char
hGetChar handle = liftRaw (HGetChar handle Pure)

-- | Read a character from the standard input device
-- (same as 'hGetChar' 'stdin').
getChar :: (Stdin :<: f) => Restr f Char
getChar = hGetChar IO.stdin

-- | Read a line from the standard input device
-- (same as 'hGetLine' 'stdin').
getLine :: (Functor f, Stdin :<: f) => Restr f String
getLine = do
  c <- getChar
  if c == '\n'
     then return ""
     else do
       cs <- getLine
       return (c:cs)

-- | Read a character from a handle.
hGetContents :: (Stdin :<: f) => Handle -> Restr f String
hGetContents handle = liftRaw (HGetContents handle Pure)

-- | Read a character from the standard input device
-- (same as 'hGetChar' 'stdin').
getContents :: (Stdin :<: f) => Restr f String
getContents = hGetContents IO.stdin

-- | The 'interact' function takes a function of type @String->String@
-- as its argument.  The entire input from the standard input device is
-- passed to this function as its argument, and the resulting string is
-- output on the standard output device.
interact :: (Stdin :<: f, Stdout :<: f, Functor f)
         => (String -> String) -> Restr f ()
interact f = do
  s <- getContents
  putStr (f s)

-- | Reading from file
data R a = Read FilePath (String -> a) deriving Functor

instance Run R where
  runAlgebra (Read file io) = P.readFile file >>= io

readFile :: (Functor f, R :<: f) => FilePath -> Restr f String
readFile skrá = liftRaw (Read skrá Pure)

-- | Writing to file
data W a
  = Write FilePath String a
  | Append FilePath String a
  deriving Functor

instance Run W where
  runAlgebra (Write file cont io) = P.writeFile file cont >> io
  runAlgebra (Append file cont io) = P.appendFile file cont >> io

-- | The computation 'writeFile' @file str@ function writes the string @str@,
-- to the file @file@.
writeFile :: (Functor f, W :<: f) => FilePath -> String -> Restr f ()
writeFile skrá cont = liftRaw (Write skrá cont (Pure ()))

-- | The computation 'appendFile' @file str@ function appends the string @str@,
-- to the file @file@.
appendFile :: (Functor f, W :<: f) => FilePath -> String -> Restr f ()
appendFile skrá cont = liftRaw (Append skrá cont (Pure ()))

-- | Capability to read and write to a file.
type RW = R :+: W

-- | File status
data Stat a = Stat FilePath (Maybe FileStatus -> a) deriving Functor

instance Run Stat where
  runAlgebra (Stat file io) = do
    status <- catch
              (Just `fmap` getFileStatus file)
              (\(_ :: SomeException) -> return Nothing)
    io status

stat :: (Functor f, Stat :<: f) => FilePath -> Restr f (Maybe FileStatus)
stat skrá = liftRaw (Stat skrá Pure)

-- | Gets directory listing
data Dir a = Dir FilePath (Maybe [FilePath] -> a) deriving Functor

instance Run Dir where
  runAlgebra (Dir file io) = do
    cont <- catch
            (Just `fmap` Dir.getDirectoryContents file)
            (\(_ :: SomeException) -> return Nothing)
    io cont

getDirectoryContents :: (Dir :<: f) => FilePath -> Restr f (Maybe [FilePath])
getDirectoryContents dir = liftRaw (Dir dir Pure)

-- | Remove files.
data Rm a
  = Rm FilePath a
  | RmDir FilePath a
  | RmRecursive FilePath a
  deriving Functor

instance Run Rm where
  runAlgebra (Rm file io)         = Dir.removeFile file              >> io
  runAlgebra (RmDir dir io)       = Dir.removeDirectory dir          >> io
  runAlgebra (RmRecursive dir io) = Dir.removeDirectoryRecursive dir >> io

rm :: (Rm :<: f) => FilePath -> Restr f ()
rm file = liftRaw $ Rm file $ Pure ()

rmDir :: (Rm :<: f) => FilePath -> Restr f ()
rmDir dir = liftRaw $ RmDir dir $ Pure ()

rmRec :: (Rm :<: f) => FilePath -> Restr f ()
rmRec dir = liftRaw $ RmRecursive dir $ Pure ()
