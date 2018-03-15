
module Environment.Theater
  (
      VerTheater,
      VerObject,
      VerMapping (..),
      readTheater
  )
where

-- own modules
import Core
import Filesystem

-- external modules
import Path
import Data.Yaml (decodeEither)
import Data.Bitraversable

-- template haskell
import Data.Aeson.TH


----------------------------------------------
-- Definition of the Theater

type Object = Either (FileMapping) (DirMapping)
type Theater = [Object]

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

type VerObject = Either (VerMapping File) (VerMapping Dir)
type VerTheater = [VerObject]

data VerMapping t = VerMapping
  {
      verEncrypted :: NonExPath,
      verEncryptedLocked :: ExPath t,
      verDecrypted :: ExPath t
  }






----------------------------------------------------------
-- The main function

readTheater :: ExPath Dir -> PartIO VerTheater
readTheater theaterDir =
    let

        --------------------------------------------------
        -- load the theater.yaml file from disk

        theaterFile :: Path Abs File
        theaterFile = theaterDir->>trustEx </> [relfile|theater.yaml|]

        theater :: PartIO Theater
        theater = (liftEither . decodeEither) =<< safeReadFile theaterFile

        --------------------------------------------------
        -- Define `left` and `right` to act on
        -- FileMapping and DirMapping respectively

        left :: FileMapping -> PartIO (VerMapping File)
        left m =
          do
              enc  <- nonExPath (m->>encryptedFile->>toFilePath)
              lock <- exFile =<< (m->>encryptedFile->>lockfilePath)
              dec  <- exFile $ theaterDir->>trustEx </> m->>decryptedFile

              return $ VerMapping enc lock dec

        right :: DirMapping -> PartIO (VerMapping Dir)
        right m =
          do
              enc  <- nonExPath (m->>encryptedDir->>toFilePath)
              lock <- exDir =<< (m->>encryptedDir->>lockdirPath)
              dec  <- exDir $ theaterDir->>trustEx </> m->>decryptedDir

              return $ VerMapping enc lock dec

        -------------------------------------------------
        -- Verify theater by executing `left` and `right`
        -- on every TheaterObject

        verTheater :: PartIO VerTheater
        verTheater = theater >>= mapM (bimapM left right)

    in verTheater






