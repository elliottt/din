{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Din (
    -- * Din Monad
    Din
  , runDin

    -- * Primitive Operations
  , io
  ) where

import Control.Applicative (Applicative(..))
import Database.SQLite (SQLiteHandle)
import MonadLib (ReaderT,ReaderM(..),BaseM(..))
import MonadLib.Derive


-- Din Environment -------------------------------------------------------------

data Env = Env
  { envDbHandle :: SQLiteHandle
  }

emptyEnv :: SQLiteHandle -> Env
emptyEnv h = Env
  { envDbHandle = h
  }


-- Din Monad -------------------------------------------------------------------

newtype Din a = Din { unDin :: ReaderT Env IO a }

iso_Din = Iso Din unDin

instance Functor Din where
  fmap = derive_fmap iso_Din

instance Applicative Din where
  pure x  = Din (pure x)
  a <*> b = Din (unDin a <*> unDin b)

instance Monad Din where
  return = derive_return iso_Din
  (>>=)  = derive_bind iso_Din
  fail   = derive_fail iso_Din

instance ReaderM Din Env where
  ask = derive_ask iso_Din

instance BaseM Din IO where
  inBase = derive_inBase iso_Din

io :: BaseM m IO => IO a -> m a
io  = inBase

runDin :: Din a -> SQLiteHandle-> IO a
runDin m h = derive_runM iso_Din m (emptyEnv h)
