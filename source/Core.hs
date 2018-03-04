
module Core
  (
      PartIO,
      liftEither,
      (->>)
  )
where

import Control.Monad.Except

type PartIO a = ExceptT String IO a

-- liftEither :: (Monad m, MonadError a (Either a)) => Either a b -> ExceptT a m b
-- liftEither = either throwError return

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return


infixl 9 ->>
(->>) :: a -> (a -> b) -> b
x ->> f = f x

