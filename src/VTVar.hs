{-# LANGUAGE MultiParamTypeClasses #-}
module VTVar
  ( VTVar
  , VTVarI
  , blockVTVar
  , blockVTVarIO
  , newVTVar
  , readVTVar
  , writeVTVar
  )
    where

import           Control.Concurrent.STM      (STM (..), TVar (..))
import qualified Control.Concurrent.STM      as S (atomically, catchSTM,
                                                   throwSTM)
import qualified Control.Concurrent.STM.TVar as S (newTVar, readTVar, writeTVar)
import           Control.Exception.Base      (Exception, SomeException (..))
import           Control.Monad               (Monad (..))
import           Data.Maybe

import           Data.Typeable

newtype VTVarVersion a = VTVarVersion Integer

newtype VTVar a = VTVar (TVar (VTVarI a))

type VTVarI a = (a, VTVarVersion a)

data VTVarAlreadyChanged = VTVarAlreadyChanged
    deriving (Show, Typeable)

instance Exception VTVarAlreadyChanged

newVTVar
    :: a
    -> STM (VTVar a)
newVTVar val = do
    var <- S.newTVar (val, VTVarVersion 0)
    return $ VTVar var

readVTVar
    :: VTVar a
    -> STM (VTVarI a)
readVTVar (VTVar var) = S.readTVar var

writeVTVar
    :: VTVar a
    -> VTVarI a
    -> STM ()
writeVTVar (VTVar var) (val, VTVarVersion varVer) = do
    VTVarVersion varVerCurr <- snd <$> S.readTVar var
    if varVer /= varVerCurr
        then S.throwSTM VTVarAlreadyChanged
        else S.writeTVar var newVar
    where
        newVar = (val, VTVarVersion newVer)
        newVer = varVer + 1

blockVTVarIO
    :: STM pureT
    -> (pureT-> IO (b, Maybe syncT))
    -> (syncT-> STM ())
    -> IO b
blockVTVarIO vars worker transaction = do
    let preLoop = do
            eres <- S.atomically $ S.catchSTM
                (Just <$> vars)
                (\VTVarAlreadyChanged-> return Nothing)
            case eres of
                Nothing -> preLoop
                Just res -> do
                    wres <- worker res
                    case wres of
                        (ret, Nothing) -> return ret
                        (ret, Just newValue) -> do
                            eres <- S.atomically $ S.catchSTM
                                (Right <$> transaction newValue)
                                (\VTVarAlreadyChanged -> return $ Left ())
                            case eres of
                                Left ()  -> preLoop
                                Right () -> return ret
    preLoop

blockVTVar
    :: STM pureT
    -> (pureT-> (b, Maybe syncT))
    -> (syncT-> STM ())
    -> IO b
blockVTVar vars worker = blockVTVarIO
    vars (return . worker)
