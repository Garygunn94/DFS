{-# LANGUAGE BangPatterns #-}
module LRUCache where
import           Control.Applicative ((<$>))
import           Data.Hashable       (Hashable, hash)
import qualified Data.HashPSQ        as HashPSQ
import           Data.IORef          (IORef, newIORef, atomicModifyIORef')
import           Data.Int            (Int64)
import           Data.Maybe          (isNothing)
import qualified Data.Vector         as V
import Prelude hiding (lookup)

type Priority = Int64

data Cache k v = Cache
    { cCapacity :: !Int       -- ^ The maximum number of elements in the queue
    , cSize     :: !Int       -- ^ The current number of elements in the queue
    , cTick     :: !Priority  -- ^ The next logical time
    , cQueue    :: !(HashPSQ.HashPSQ k Priority v)
    } deriving (Eq, Show)

empty :: Int -> Cache k v
empty capacity
    | capacity < 1 = error "Cache.empty: capacity < 1"
    | otherwise    = Cache
        { cCapacity = capacity
        , cSize     = 0
        , cTick     = 0
        , cQueue    = HashPSQ.empty
        }

trim :: (Hashable k, Ord k) => Cache k v -> Cache k v
trim c
    | cTick c == maxBound = empty (cCapacity c)
    | cSize c > cCapacity c = c
        { cSize  = cSize c - 1
        , cQueue = HashPSQ.deleteMin (cQueue c)
        }
    | otherwise = c

insert :: (Hashable k, Ord k) => k -> v -> Cache k v -> Cache k v
insert key val c = trim $!
    let (mbOldVal, queue) = HashPSQ.insertView key (cTick c) val (cQueue c)
    in c
        { cSize  = if isNothing mbOldVal then cSize c + 1 else cSize c
        , cTick  = cTick c + 1
        , cQueue = queue
        }

lookup
    :: (Hashable k, Ord k) => k -> Cache k v -> Maybe (v, Cache k v)
lookup k c = case HashPSQ.alter lookupAndBump k (cQueue c) of
    (Nothing, _) -> Nothing
    (Just x, q)  ->
        let !c' = trim $ c {cTick = cTick c + 1, cQueue = q}
        in Just (x, c')
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x,  Just ((cTick c), x))



newtype Handle k v = Handle (IORef (Cache k v))
newHandle :: Int -> IO (Handle k v)
newHandle capacity = Handle <$> newIORef (empty capacity)

iolookup ::  (Hashable k, Ord k) =>  Handle k v -> k -> IO(Maybe (v))
iolookup (Handle ref) k = do
  lookupRes <- atomicModifyIORef' ref $ \c -> case lookup k c of
       Nothing      -> (c, Nothing)
       Just (v, c') -> (c', Just v)
       
  return lookupRes
   
ioinsert :: (Hashable k, Ord k) => Handle k v -> k -> v -> IO()
ioinsert (Handle ref) k v = do
  atomicModifyIORef' ref $ \c -> (insert k v c, ())
