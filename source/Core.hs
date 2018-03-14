
module Core
  (
      PartIO,
      liftEither,
      (->>),
      (´),
      module Control.Monad.Except,
      module Control.Monad.Catch
  )
where

import Control.Monad.Except
import Control.Monad.Catch

type PartIO a = ExceptT String IO a

-- liftEither :: (Monad m, MonadError a (Either a)) => Either a b -> ExceptT a m b
-- liftEither = either throwError return

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return


infixl 9 ->>
(->>) :: a -> (a -> b) -> b
x ->> f = f x


(´) = flip (.)

