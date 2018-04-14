{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Error where

import Protolude hiding (Type)
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
  = Mismatch Text Text
  | MultipleDefinition Text Int
  | NotDefined Text
  | ParseError Text
  | BadCallError Int Int
  | OtherError Text
  deriving Show

formatError :: Error -> Text
formatError (ParseError e) = "Parsing error: " <> e
formatError (Mismatch exp act) = "Type mismatch: Expected " <> show exp <> " but got " <> show act
formatError (OtherError e) = e
formatError e = show e

warn :: MonadWriter [Warning] m => Warning -> m ()
warn w = tell [w]
