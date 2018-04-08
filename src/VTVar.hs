{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
module VTVar
  ( VTVar
  , VTVarI
  , rcuVTVar
  , ruVTVar
  , newVTVar
  , readVTVar
  , writeVTVar
  , ReadSTM
  , runReadSTM
  , atomicallyW
  , atomicallyR
  , runWSTM
  , runRSTM
  , RSTM
  , WSTM
  )
    where

import           Control.Concurrent.STM      (STM (..), TVar (..))
import qualified Control.Concurrent.STM      as S (atomically, catchSTM,
                                                   throwSTM)
import qualified Control.Concurrent.STM.TVar as S (newTVar, readTVar, writeTVar)
import           Control.Exception.Base      (Exception, SomeException (..))
import           Control.Monad               (Monad (..), when)

import           Control.Monad.Catch         (MonadCatch)
import           Control.Monad.Catch         as E (throwM, try)
import           Control.Monad.IO.Class      as IO (MonadIO, liftIO)
import           Control.Monad.Trans         (lift)
import           Data.Typeable               (Typeable)

{--| VTVarVersion
 - this type represents current version value for VTVar. For now it is an
 - Integer value (for simplicity)
 -}
newtype VTVarVersion a = VTVarVersion Integer
{--| VTVarI
 - this data type represents value and it's current version
 -}
type VTVarI a = (a, VTVarVersion a)

{--|
 - and versioned TVar is just TVar with value and version of value
 -}
newtype VTVar a = VTVar (TVar (VTVarI a))

data VTVarAlreadyChanged = VTVarAlreadyChanged
    deriving (Show, Typeable)
instance Exception VTVarAlreadyChanged

{--| ReadSTM a
 - this is a subset of STM, that is only allowed to do read operations
 -}
newtype ReadSTM a = ReadSTM { runReadSTM:: STM a}
  deriving
    ( Monad
    , Applicative
    , Functor
    )

{--| RSTM a
 - RSTM is a subset of IO, that is only allowed to use atomicallyR action
 -}
newtype RSTM a = RSTM {runRSTM:: IO a}
  deriving
    ( Monad
    , Applicative
    , Functor
    )
{--| WriteSTM a
 - this is a subset of STM, that is only allowed to do write/create operations
 -}
newtype WriteSTM a = WriteSTM { runWriteSTM:: STM a}
  deriving
    ( Monad
    , Applicative
    , Functor
    )

{--| WSTM a
 - WSTM is a subset of IO, that is only allowed to use atomicallyW action
 -}
newtype WSTM a = WSTM {runWSTM:: IO a}
  deriving
    ( Monad
    , Applicative
    , Functor
    )


{--| newVTVar value
 - creates new versioned TVar with given initial value. Only valid in
 -}
newVTVar:: a-> WriteSTM (VTVar a)
newVTVar val = WriteSTM $ do
  var <- S.newTVar (val, VTVarVersion 0)
  return $ VTVar var
{--| writeVTVar var (value,knownversion)
 - this function tries to update variable var with new value, but will throw
 - VTVarAlreadyChanged exception if knownversion /= actual version. This will
 - trigger blockVTVar to retry computations
 -}
writeVTVar:: VTVar a-> VTVarI a-> WriteSTM ()
writeVTVar (VTVar var) (val, VTVarVersion varVer) = WriteSTM $ do
  VTVarVersion varVerCurr <- snd <$> S.readTVar var
  if varVer /= varVerCurr
      then S.throwSTM VTVarAlreadyChanged
      else S.writeTVar var newVar
  where
      newVar = (val, VTVarVersion newVer)
      newVer = varVer + 1
{--| guardUntouched var knownversion
 - just checks if variable had been changed from known version
 -}
guardUntouched:: VTVar a-> VTVarVersion a-> WriteSTM ()
guardUntouched (VTVar var) (VTVarVersion varVer) = WriteSTM $ do
  VTVarVersion varVerCurr <- snd <$> S.readTVar var
  when (varVer /= varVerCurr ) $  S.throwSTM VTVarAlreadyChanged

{--| atomicallyW actions
 - perform atomic write/create operations. if VTVarAlreadyChanged exception had
 - been raised during actions execution, will rethrow it.
 -}
atomicallyW:: WriteSTM a-> WSTM a
atomicallyW action = WSTM $ do
  eret <- S.atomically $ S.catchSTM ( Right <$> runWriteSTM action)
    (\VTVarAlreadyChanged-> return $ Left VTVarAlreadyChanged)
  case eret of
    Left VTVarAlreadyChanged -> throwM VTVarAlreadyChanged
    Right some               -> return some

{--| readVTVar var
 - reads current value of the variable
 -}
readVTVar:: VTVar a-> ReadSTM (VTVarI a)
readVTVar (VTVar var) = ReadSTM $ S.readTVar var

{--| atomicallyR actions
 - perform read atomic operations
 -}
atomicallyR:: ReadSTM a-> RSTM a
atomicallyR action = RSTM $ S.atomically $ runReadSTM action

{--| rcuVTVar readvars compute write
 - this function represents use case of reading variables, compute something
 - and update values of some variables.
 - readvars is in RSTM monad, which means, that it can consists from multiple
 - calls to atomicallyR.
 - if compute's second return value of the pair is Just some - then this "some"
 - value will be passed to "write" action.
 - if write action will try to operate with variable, which has been already
 - changed, then the write transaction will be aborted and the whole computation
 - will be restarted (ie, readvars will be executed again).
 -
 - The idea is that reading/writing STM actions should be fast and should not
 - contain heavy computations. Ideally, heavy computation should be performed
 - in compute stage. This is needed to keep STM journal as small/fast as
 - possible, to not impact application's performance
 -}
rcuVTVar
  :: ( MonadIO m
     , MonadCatch m
     )
  => RSTM vars
  -> ( vars-> (a, Maybe modifiedVars))
  -> ( modifiedVars-> WriteSTM b)
  -> m (a, Maybe b)
rcuVTVar readvarsM workerM transactionM = loop
  where
    loop = do
      vars <- liftIO $ runRSTM readvarsM
      let (value, mmodifiedVars) = workerM vars
      case mmodifiedVars of
        Nothing-> return (value, Nothing)
        Just modifiedVars -> do
          eres <- E.try $ liftIO $ runWSTM $ atomicallyW $ transactionM modifiedVars
          case eres of
            Left VTVarAlreadyChanged -> loop
            Right some               -> return (value, Just some)

{--| ruVTVar readvars writevars
 - the same as rcuVTVars, but calculate step is included in the readvars stage
 -}
ruVTVar
  :: ( MonadIO m
     , MonadCatch m
     )
  => RSTM (a, Maybe modifyVars)
  -> (modifyVars-> WriteSTM b)
  -> m (a, Maybe b)
ruVTVar readvarsM writevarsM = loop
  where
    loop = do
      (value, mmodifiedVars) <- liftIO $ runRSTM readvarsM
      case mmodifiedVars of
        Nothing-> return (value, Nothing)
        Just modifiedVars -> do
          eres <- E.try $ liftIO $ runWSTM $ atomicallyW $ writevarsM modifiedVars
          case eres of
            Left VTVarAlreadyChanged -> loop
            Right some               -> return (value, Just some)
