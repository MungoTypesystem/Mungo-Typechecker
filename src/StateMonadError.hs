module StateMonadError where

import qualified Control.Monad.Fail as Fail
import Control.Monad (ap, liftM, guard, liftM2, forM, when)

data MState s a = MState { runState :: s -> Either String (a, s)}
                | MStateError String

instance Show (MState s a) where
    show (MState _)   = "mstate"
    show (MStateError err) = "mstate err " ++ err 

instance Functor (MState s) where
    fmap = Control.Monad.liftM

instance Applicative (MState s) where
    pure  = return
    (<*>) = Control.Monad.ap

instance Monad (MState s) where
    return x         = MState $ \s -> Right (x, s)
    fail str         = MStateError str
    (MState h) >>= f = MState $ \s -> do (a, newState) <- h s 
                                         case f a of
                                            (MState g) -> g newState
                                            (MStateError err) -> Left err
    (MStateError str) >>= f = MStateError str                                  

instance Fail.MonadFail (MState s) where
    fail str         = MStateError str

getState :: MState s s
getState = MState $ \m -> Right (m, m)

setState :: s -> MState s ()
setState v = MState $ \m -> Right ((), v)


headM :: Monad m => [a] -> m a
headM (x:_) = return x
headM []    = fail ""

lastM :: Monad m => [a] -> m a
lastM [] = fail "" 
lastM xs = return $ last xs

initM :: Monad m => [a] -> m [a]
initM [] = fail ""
initM xs = return $ init xs


fromEitherM :: Monad m => Either String b -> m b
fromEitherM (Right x) = return x
fromEitherM (Left x)  = fail x

fromMaybeM :: Monad m => Maybe b -> m b
fromMaybeM (Just x) = return x
fromMaybeM _        = fail ""

assert' :: Monad m => Bool -> m ()
assert' True  = return ()
assert' False = fail "" 

envLookupIn :: (Monad m, Eq a) => a -> [(a, b)] -> m b
envLookupIn k l = fromMaybeM (k `lookup` l)
