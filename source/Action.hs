
module Action
  (
      openFile
  )
where

-- own modules
import Core
import Environment
import Environment.Theater
import Filesystem

-- external modules
import System.Command (readProcessWithExitCode, isFailure)
import Path
import Path.IO
-- (for password)
import System.Console.Haskeline


cipher :: String
cipher = "AES265"

lockFile :: ExPath File -> IO (ExPath File, NonExPath)
lockFile f =
  do
      let file = f->>trustEx

      lock <- file->>lockfilePath
      renameFile file lock
      (,) <$> exFile lock <*> nonExPath (file->>toFilePath)

decryptFile :: ExPath File -> ExPath Dir -> PartIO (ExPath File)
decryptFile encrypted targetDir =
  do
      let encryptedFile = encrypted->>trustEx
          targetFile = targetDir->>trustEx </> (encrypted->>trustEx->>filename)

      pass <- getPassphrase

      lift $ putStrLn $ "Decrypting " ++ encryptedFile->>toFilePath ++ " into " ++ targetFile->>toFilePath

      let arguments = ["-d",
                       "-o", targetFile->>toFilePath,
                       "--passphrase", pass,
                       encryptedFile->>toFilePath]

      (code, out, err) <- lift $ readProcessWithExitCode "gpg" arguments ""

      lift $ putStrLn $ "OUT: " ++ out
      lift $ putStrLn $ "ERR: " ++ err

      if (code->>isFailure) then throwError "Decryption failed"
                            else exFile targetFile



openFile ::  ExPath Dir -> ExPath File -> PartIO VerObject
openFile target encrypted =
  do
      (lock,oldname)    <- lift $ lockFile encrypted
      decrypted <- decryptFile lock target
      return (Left (VerMapping oldname lock decrypted))

-------------------------
-- getting passwd

getPassphrase :: PartIO String
getPassphrase =
  do
      input <- lift $ runInputT defaultSettings (getPassword (Just '.') "Passphrase: ")
      case input of
        Just s -> return s
        Nothing -> throwError "Could not read password."

