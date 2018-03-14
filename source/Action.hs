
module Action
  (
      openFile
  )
where

import Core
import Environment
import Environment.Theater
import Filesystem

import Path
import Path.IO


cipher :: String
cipher = "AES265"

lockFile :: ExPath File -> IO (ExPath File, NonExPath)
lockFile f =
  do
      let file = f->>trustEx

      lock <- file->>lockfilePath
      renameFile file lock
      (,) <$> exFile lock <*> nonExPath (file->>toFilePath)

decryptFile :: ExPath File -> ExPath Dir -> IO (ExPath File)
decryptFile encrypted target = undefined
  -- do
  --     putStrLn "decrypting..."


openFile ::  ExPath Dir -> ExPath File -> PartIO VerObject
openFile target encrypted =
  do
      (lock,oldname)    <- lift $ lockFile encrypted
      decrypted <- lift $ decryptFile lock target
      return (Left (VerMapping oldname lock decrypted))
