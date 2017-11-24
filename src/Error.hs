{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Error where

import Type
import Control.Monad.Except
import Control.Monad.Writer
import qualified Data.Text as T

newtype ErrWarn a = ErrWarn { unErrWarn :: WriterT [Warning] (ExceptT Error IO) a }
  deriving (Functor, Applicative, Monad, MonadWriter [Warning], MonadError Error)

runErrWarn :: ErrWarn a -> IO (Either Error a)
runErrWarn ew = do
  res <- runExceptT . runWriterT $ unErrWarn ew
  case res of
    Left e -> return $ Left e
    Right (a, w) -> do
      mapM_ print w
      return $ Right a

data Warning
  = UnusedVariable T.Text
  deriving Show

data Error
  = Mismatch Type Type
  | MultipleDefinition T.Text Int
  | NotDefined T.Text
  | ParseError T.Text
  | OtherError T.Text
  deriving Show

warn :: MonadWriter [Warning] m => Warning -> m ()
warn w = tell [w]
