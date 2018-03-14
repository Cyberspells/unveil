
module Error where

import Control.Monad.Catch
import Data.Typeable

data UException =
    PathDoesNotExist String
    | FileDoesNotExist String
    | DirDoesNotExist String
    | PathShouldNotExist String
    deriving (Typeable, Show)

instance Exception UException
