
module Theater where

import Core
import Environment
import Path
import Data.Yaml (decodeEither)
import Data.Aeson.TH
import Filesystem
import Environment
import Control.Monad.Except
import Data.Bifunctor
import Data.Bitraversable

-- data Theater = Theater
--   {
--       mappings :: [Mapping]
--   }

type Theater = [Either (FileMapping) (DirMapping)]

data FileMapping = FileMapping
  {
    encryptedFile :: Path Abs File,
    decryptedFile :: Path Rel File
  }

data DirMapping = DirMapping
  {
    encryptedDir :: Path Abs Dir,
    decryptedDir :: Path Rel Dir
  }


$(deriveJSON defaultOptions ''FileMapping)
$(deriveJSON defaultOptions ''DirMapping)

type VerTheater = [Either (VerMapping File) (VerMapping Dir)]

data VerMapping t = VerMapping
  {
      verEncrypted :: NonExPath,
      verEncryptedLocked :: ExPath t,
      verDecrypted :: ExPath t
  }

-- data Mapping =
--     FileMapping (Path Rel File) (Path Abs File)
--     | DirMapping (Path Rel Dir) (Path Abs Dir)

-- data VerMapping =
--     VerFileMapping (ExFile) Path


theaterDir :: Environment -> ExPath Dir
theaterDir = verTheater . config

readTheaterFile :: Environment -> PartIO Theater
readTheaterFile env =
  do
      (liftEither . decodeEither) =<< safeReadFile theaterFile

  where
      theaterFile = env->>theaterDir->>unver </> [relfile|theater.yaml|]

verifyTheater :: Environment -> Theater -> PartIO VerTheater
verifyTheater env = mapM $ bimapM l r
  where
    l :: FileMapping -> PartIO (VerMapping File)
    l m =
      do
          enc  <- nonExPath (m->>encryptedFile->>toFilePath)
          lock <- exFile =<< (m->>encryptedFile <.> "lock")
          dec  <- exFile $ env->>theaterDir->>unver </> m->>decryptedFile

          return $ VerMapping enc lock dec

    r :: DirMapping -> PartIO (VerMapping Dir)
    r m =
      do
          enc  <- nonExPath (m->>encryptedDir->>toFilePath)
          lock <- exDir =<< (m->>encryptedDir->>rename (++ ".lock"))
          dec  <- exDir $ env->>theaterDir->>unver </> m->>decryptedDir

          return $ VerMapping enc lock dec

          where
            rename :: (String -> String) -> Path b Dir -> PartIO (Path b Dir)
            rename f d = (d->>parent </>) <$> parseRelDir (f (d->>dirname->>toFilePath))



-- verifyTheater :: Theater -> VerTheater



