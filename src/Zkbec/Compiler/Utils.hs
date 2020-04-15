{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zkbec.Compiler.Utils where

import           Zkbec.Prelude hiding (Ap)

import           Control.Applicative.Free as Ap
import           Control.Monad.Free
import qualified Data.IntMap.Strict       as IntMap
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import           Data.Vector              (Vector)
import qualified Data.Vector              as Vector
import           System.Random

infixl 1 `through`

randomRM :: (RandomGen g, Random a) => (a, a) -> State g a
randomRM = state . randomR

randomM :: (RandomGen g, Random a) => State g a
randomM = state random

instance (Random x, Random y) => Random (x, y) where
    randomR ((x1, y1), (x2, y2)) = runState $ (,) <$> randomRM (x1, x2) <*> randomRM (y1, y2)
    random = runState $ (,) <$> randomM <*> randomM

instance Random Rational where
    randomR = error "Not implemented."
    random = runState $ do
        (n, d) <- randomM
        if d == 0
            then randomM
            else return $ n % d

data RequestF req resp r = RequestF req (resp -> r)

data HRequestF req resp r = forall a. HRequestF (req a) (resp a -> r)

instance Functor (RequestF req resp) where
    fmap f (RequestF req cont) = RequestF req $ f . cont

instance Functor (HRequestF req resp) where
    fmap f (HRequestF req cont) = HRequestF req $ f . cont

class LiftFree t where
    liftFree :: Functor f => f a -> t f a

instance LiftFree Free where
    liftFree = liftF

instance LiftFree Ap where
    liftFree = liftAp

request :: LiftFree t => req -> t (RequestF req resp) resp
request req = liftFree $ RequestF req id

hrequest :: LiftFree t => req a -> t (HRequestF req resp) (resp a)
hrequest req = liftFree $ HRequestF req id

runRequests :: (req -> resp) -> Free (RequestF req resp) a -> a
runRequests respond = iter $ \(RequestF req cont) -> cont $ respond req

runRequestsM :: Monad m => (req -> m resp) -> Free (RequestF req resp) a -> m a
runRequestsM respond = iterA $ \(RequestF req cont) -> respond req >>= cont

runCollectRequests :: (req -> resp) -> Free (RequestF req resp) a -> ([req], a)
runCollectRequests respond = runRequestsM $ \req -> ([req], respond req)

runRequestsA :: Applicative f => (req -> f resp) -> Ap (RequestF req resp) a -> f a
runRequestsA respond = runAp $ \(RequestF req cont) -> cont <$> respond req

runCollectRequestsA :: (req -> resp) -> Ap (RequestF req resp) a -> ([req], a)
runCollectRequestsA respond = runRequestsA $ \req -> ([req], respond req)

runCollectRequestsDelayA :: Ap (RequestF req resp) a -> ([req], [resp] -> Maybe a)
runCollectRequestsDelayA a = case go a of
    (reqs, getX) -> (,) reqs $ \resps -> do
        (x, resps') <- runStateT getX resps
        if null resps' then Just x else Nothing
  where
    go :: Ap (RequestF req resp) a -> ([req], StateT [resp] Maybe a)
    go (Ap.Pure x)                   = ([], pure x)
    go (Ap (RequestF req cont) rest) = case go rest of
        (reqs, getF) -> (,) (req : reqs) $ do
            resp <- StateT uncons
            f <- getF
            pure . f $ cont resp

runHRequests :: (forall a. req a -> resp a) -> Free (HRequestF req resp) b -> b
runHRequests respond = iter $ \(HRequestF req cont) -> cont $ respond req

runHRequestsM
    :: Monad m => (forall a. req a -> m (resp a)) -> Free (HRequestF req resp) b -> m b
runHRequestsM respond = iterA $ \(HRequestF req cont) -> respond req >>= cont

unzipF :: Functor f => f (a, b) -> (f a, f b)
unzipF p = (fst <$> p, snd <$> p)

tell :: (MonadState s m, Monoid s) => s -> m ()
tell s = modify (<> s)

through :: Monad m => m a -> (a -> m b) -> m a
through a f = a >>= \x -> x <$ f x

revertIntMap :: IntMap Int -> IntMap Int
revertIntMap = IntMap.fromList . map swap . IntMap.toList

atDef :: a -> Int -> Vector a -> a
atDef x i xs = fromMaybe x $ xs Vector.!? i

tryPrepad :: [a] -> b -> [b] -> Maybe [(a, b)]
tryPrepad xs y ys = do
    let xsL = length xs
        ysL = length ys
    guard $ xsL >= ysL
    Just . zip xs $ replicate (xsL - ysL) y ++ ys

tryPostpad :: [a] -> b -> [b] -> Maybe [(a, b)]
tryPostpad xs y ys = do
    guard $ length xs >= length ys
    Just . zip xs $ ys ++ repeat y

reorder :: Ord a => [a] -> [(a, b)] -> [b]
reorder xs xys = map (\x -> Map.fromList xys Map.! x) xs

-- | Check that all elements of a list are distinct.
distinct :: Ord a => [a] -> Bool
distinct xs = length (Set.fromList xs) == length xs
