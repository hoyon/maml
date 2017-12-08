{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Error where

import Protolude
import Type
import Control.Monad.Writer

newtype ErrWarn a = ErrWarn { unErrWarn :: WriterT [Warning] (ExceptT Error IO) a }
  deriving (Functor, Applicative, Monad, MonadWriter [Warning], MonadError Error)

runErrWarn :: ErrWarn a -> IO (Either Error a)
runErrWarn ew = do
  res <- runExceptT . runWriterT $ unErrWarn ew
  case res of
    Left e -> return $ Left e
    Right (a, w) -> do
      mapM_ putWarn w
      return $ Right a

putWarn :: Warning -> IO ()
putWarn = hPutStrLn stderr . (show :: Warning -> Text)

data Warning
  = UnusedVariable Text
  deriving Show

data Error
  = Mismatch Type Type
  | MultipleDefinition Text Int
  | NotDefined Text
  | ParseError Text
  | OtherError Text
  deriving Show

warn :: MonadWriter [Warning] m => Warning -> m ()
warn w = tell [w]