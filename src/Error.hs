{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Error where

import Protolude hiding (Type)
import Type
import Control.Monad.Writer
import Control.Monad.Reader
import WasmParse

newtype ErrWarn a = ErrWarn { unErrWarn :: ReaderT Wasm (WriterT [Warning] (ExceptT Error IO)) a }
  deriving (Functor, Applicative, Monad, MonadWriter [Warning], MonadError Error, MonadReader Wasm)

runErrWarn :: Wasm -> ErrWarn a -> IO (Either Error a)
runErrWarn wasm ew = do
  res <- runExceptT . runWriterT $ runReaderT (unErrWarn ew) wasm
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
  | BadCallError Int Int
  | OtherError Text
  deriving Show

warn :: MonadWriter [Warning] m => Warning -> m ()
warn w = tell [w]
