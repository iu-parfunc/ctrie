{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, PatternGuards, MagicHash #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-----------------------------------------------------------------------
-- | A non-blocking concurrent map from hashable keys to values.
--
-- The implementation is based on /lock-free concurrent hash tries/
-- (aka /Ctries/) as described by:
--
--    * Aleksander Prokopec, Phil Bagwell, Martin Odersky,
--      \"/Cache-Aware Lock-Free Concurent Hash Tries/\"
--    * Aleksander Prokopec, Nathan G. Bronson, Phil Bagwell, Martin
--      Odersky \"/Concurrent Tries with Efficient Non-Blocking Snapshots/\"
--
-- Operations have a worst-case complexity of /O(log n)/, with a base
-- equal to the size of the native 'Word'.
--
-----------------------------------------------------------------------

module Control.Concurrent.Map
    ( Map

      -- * Construction
    , empty

      -- * Modification
    , insert
    , delete
    , insertIfAbsent
    , putIfAbsent
    , PutResult(..)

      -- * Query and traversal
    , lookup
    , unsafeTreeTraverse
    , unsafeTreeTraverse'

      -- * Lists
    , fromList
    , unsafeToList
    , foldlWithKey

    --, printMap
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Atomics
import Data.Bits
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import Data.IORef
import qualified Data.List as List
import Data.Maybe
import Data.Word
import Prelude hiding (lookup)

import qualified Control.Concurrent.Map.Array as A

import Control.Concurrent.Map.Internal

-----------------------------------------------------------------------

isTomb :: MainNode k v -> Bool
isTomb (Tomb _) = True
isTomb _        = False

hash :: Hashable a => a -> Hash
hash = fromIntegral . H.hash


-----------------------------------------------------------------------
-- * Construction

-- | /O(1)/. Construct an empty map.
empty :: IO (Map k v)
empty = Map <$> newIORef (CNode 0 A.empty)


-----------------------------------------------------------------------

{-# INLINE unsafeTreeTraverse #-}
-- | Perform a traversal of the internal tree structure without revealing
-- the exact details of the implementation.
unsafeTreeTraverse :: MonadIO m
                   => Map k v
                   -> (k -> v -> m ())
                   -> (Int -> (Int -> m ()) -> m ())
                   -> m ()
unsafeTreeTraverse (Map root) doElem doSplit = go root
  where
    go inode = do
        main <- liftIO $ readIORef inode
        case main of
            CNode bmp arr -> doSplit (popCount bmp)
                                     (\ix -> go2 (A.index arr ix))
            Tomb (S k v)  -> doElem k v
            Collision xs  -> go4 xs

    go2 (INode inode)   = go inode
    go2 (SNode (S k v)) = doElem k v

    -- Possible minor optimization:
    -- go3 []                = return ()
    -- go3 [(SNode (S k v))] = doElem k v
    -- go3 [(SNode (S k1 v1)),(SNode (S k2 v2))] = do doElem k1 v1; doElem k2 v2
    -- go3 ls = go4 ls

    go4 []  = return ()
    go4 ((S k v):rst) = doSplit 2 (\n -> case n of
                                          0 -> doElem k v
                                          1 -> go4 rst
                                          _ -> error $
                                               "unsafeTreeTraverse: index ("++show n++
                                               ") is out of bounds for the 2-way split")

unsafeTreeTraverse' :: MonadIO m
                    => Map k v
                    -> (k -> v -> m a)
                    -> (Int -> (Int -> m a) -> m a)
                    -> a
                    -> m a
unsafeTreeTraverse' (Map root) doElem doSplit acc = go root
  where
    go inode = do
        main <- liftIO $ readIORef inode
        case main of
            CNode bmp arr -> doSplit (popCount bmp)
                                     (\ix -> go2 (A.index arr ix))
            Tomb (S k v)  -> doElem k v
            Collision xs  -> go4 xs

    go2 (INode inode)   = go inode
    go2 (SNode (S k v)) = doElem k v

    go4 []  = return acc
    go4 ((S k v):rst) = doSplit 2 (\n -> case n of
                                          0 -> doElem k v
                                          1 -> go4 rst
                                          _ -> error $
                                               "unsafeTreeTraverse': index ("++show n++
                                               ") is out of bounds for the 2-way split")

-----------------------------------------------------------------------
-- * Modification

-- | /O(log n)/. Associate the given value with the given key.
-- If the key is already present in the map, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> Map k v -> IO ()
insert k v (Map root) = go0
    where
        h = hash k
        go0 = go 0 undefined root
        go lev parent inode = do
            ticket <- readForCAS inode
            case peekTicket ticket of
                CNode bmp arr -> do
                    let m = mask h lev
                        i = sparseIndex bmp m
                        n = popCount bmp
                    if bmp .&. m == 0
                        then do
                            let arr' = A.insert (SNode (S k v)) i n arr
                                cn'  = CNode (bmp .|. m) arr'
                            unlessM (fst <$> casIORef inode ticket cn') go0

                        else case A.index arr i of
                            SNode (S k2 v2)
                                | k == k2 -> do
                                    let arr' = A.update (SNode (S k v)) i n arr
                                        cn'  = CNode bmp arr'
                                    unlessM (fst <$> casIORef inode ticket cn') go0

                                | otherwise -> do
                                    let h2 = hash k2
                                    inode2 <- newINode h k v h2 k2 v2 (nextLevel lev)
                                    let arr' = A.update (INode inode2) i n arr
                                        cn'  = CNode bmp arr'
                                    unlessM (fst <$> casIORef inode ticket cn') go0

                            INode inode2 -> go (nextLevel lev) inode inode2

                Tomb _ -> clean parent (prevLevel lev) >> go0

                Collision arr -> do
                    let arr' = S k v : filter (\(S k2 _) -> k2 /= k) arr
                        col' = Collision arr'
                    unlessM (fst <$> casIORef inode ticket col') go0

{-# INLINABLE insert #-}

data PutResult v = Added v | Found v

-- | /O(log n)/. Associate the given value with the given key.
-- If the key is already present in the map, don't change the value.
insertIfAbsent :: (Eq k, Hashable k) => k -> v -> Map k v -> IO (PutResult v)
insertIfAbsent k v (Map root) = go0
    where
        h = hash k
        go0 = go 0 undefined root
        go lev parent inode = do
            ticket <- readForCAS inode
            case peekTicket ticket of
                CNode bmp arr -> do
                    let m = mask h lev
                        i = sparseIndex bmp m
                        n = popCount bmp
                    if bmp .&. m == 0
                        then do
                            let arr' = A.insert (SNode (S k v)) i n arr
                                cn'  = CNode (bmp .|. m) arr'
                            unlessM' v (fst <$> casIORef inode ticket cn') go0

                        else case A.index arr i of
                            SNode (S k2 v2)
                                | k == k2 -> return $ Found v

                                | otherwise -> do
                                    let h2 = hash k2
                                    inode2 <- newINode h k v h2 k2 v2 (nextLevel lev)
                                    let arr' = A.update (INode inode2) i n arr
                                        cn'  = CNode bmp arr'
                                    unlessM' v (fst <$> casIORef inode ticket cn') go0

                            INode inode2 -> go (nextLevel lev) inode inode2

                Tomb _ -> clean parent (prevLevel lev) >> go0

                Collision arr ->
                    if any (\(S k2 _) -> k2 == k) arr
                        then return $ Found v
                        else do
                            let arr' = S k v : filter (\(S k2 _) -> k2 /= k) arr
                                col' = Collision arr'
                            unlessM' v (fst <$> casIORef inode ticket col') go0
              where
                unlessM' v' p s = p >>= \t -> if t then return (Added v') else s

{-# INLINABLE insertIfAbsent #-}

putIfAbsent :: (Ord k, Hashable k, MonadIO m) =>
               Map k v           -- ^ The map
               -> k              -- ^ The key to lookup/insert
               -> m v            -- ^ A computation of the value to insert
               -> m (PutResult v)
putIfAbsent m k vc = do
  v <- vc
  liftIO $ insertIfAbsent k v m

{-# INLINABLE putIfAbsent #-}

newINode :: Hash -> k -> v -> Hash -> k -> v -> Int -> IO (INode k v)
newINode h1 k1 v1 h2 k2 v2 lev
    | lev >= hashLength = newIORef $ Collision [S k1 v1, S k2 v2]
    | otherwise = do
        let i1 = index h1 lev
            i2 = index h2 lev
            bmp = (unsafeShiftL 1 i1) .|. (unsafeShiftL 1 i2)
        case compare i1 i2 of
            LT -> newIORef $ CNode bmp $ A.pair (SNode (S k1 v1)) (SNode (S k2 v2))
            GT -> newIORef $ CNode bmp $ A.pair (SNode (S k2 v2)) (SNode (S k1 v1))
            EQ -> do inode' <- newINode h1 k1 v1 h2 k2 v2 (nextLevel lev)
                     newIORef $ CNode bmp $ A.singleton (INode inode')


-- | /O(log n)/. Remove the given key and its associated value from the map,
-- if present.
delete :: (Eq k, Hashable k) => k -> Map k v -> IO ()
delete k (Map root) = go0
    where
        h = hash k
        go0 = go 0 undefined root
        go lev parent inode = do
            ticket <- readForCAS inode
            case peekTicket ticket of
                CNode bmp arr -> do
                    let m = mask h lev
                        i = sparseIndex bmp m
                    if bmp .&. m == 0
                        then return ()  -- not found
                        else case A.index arr i of
                            SNode (S k2 _)
                                | k == k2 -> do
                                    let arr' = A.delete i (popCount bmp) arr
                                        cn'  = CNode (bmp `xor` m) arr'
                                        cn'' = contract lev cn'
                                    unlessM (fst <$> casIORef inode ticket cn'') go0
                                    whenM (isTomb <$> readIORef inode) $
                                        cleanParent parent inode h (prevLevel lev)

                                | otherwise -> return ()  -- not found

                            INode inode2 -> go (nextLevel lev) inode inode2

                Tomb _ -> clean parent (prevLevel lev) >> go0

                Collision arr -> do
                    let arr' = filter (\(S k2 _) -> k2 /= k) $ arr
                        col' | [s] <- arr' = Tomb s
                             | otherwise   = Collision arr'
                    unlessM (fst <$> casIORef inode ticket col') go0

{-# INLINABLE delete #-}

-----------------------------------------------------------------------
-- * Query

-- | /O(log n)/. Return the value associated with the given key, or 'Nothing'.
lookup :: (Eq k, Hashable k) => k -> Map k v -> IO (Maybe v)
lookup k (Map root) = go0
    where
        h = hash k
        go0 = go 0 undefined root
        go lev parent inode = do
            main <- readIORef inode
            case main of
                CNode bmp arr -> do
                    let m = mask h lev
                        i = sparseIndex bmp m
                    if bmp .&. m == 0
                        then return Nothing
                        else case A.index arr i of
                            INode inode2 -> go (nextLevel lev) inode inode2
                            SNode (S k2 v) | k == k2   -> return (Just v)
                                           | otherwise -> return Nothing

                Tomb _ -> clean parent (prevLevel lev) >> go0

                Collision xs -> do
                    case List.find (\(S k2 _) -> k2 == k) xs of
                        Just (S _ v) -> return (Just v)
                        _            -> return Nothing

{-# INLINABLE lookup #-}

-----------------------------------------------------------------------
-- * Internal compression operations

clean :: INode k v -> Level -> IO ()
clean inode lev = do
    ticket <- readForCAS inode
    case peekTicket ticket of
        cn@(CNode _ _) -> do
            cn' <- compress lev cn
            void $ casIORef inode ticket cn'
        _ -> return ()
{-# INLINE clean #-}

cleanParent :: INode k v -> INode k v -> Hash -> Level -> IO ()
cleanParent parent inode h lev = do
    ticket <- readForCAS parent
    case peekTicket ticket of
        cn@(CNode bmp arr) -> do
            let m = mask h lev
                i = sparseIndex bmp m
            unless (bmp .&. m == 0) $
                case A.index arr i of
                    INode inode2 | inode2 == inode ->
                        whenM (isTomb <$> readIORef inode) $ do
                            cn' <- compress lev cn
                            unlessM (fst <$> casIORef parent ticket cn') $
                                cleanParent parent inode h lev
                    _ -> return ()
        _ -> return ()

compress :: Level -> MainNode k v -> IO (MainNode k v)
compress lev (CNode bmp arr) =
    contract lev <$> CNode bmp <$> A.mapM resurrect (popCount bmp) arr
compress _ x = return x
{-# INLINE compress #-}

resurrect :: Branch k v -> IO (Branch k v)
resurrect b@(INode inode) = do
    main <- readIORef inode
    case main of
        Tomb s -> return (SNode s)
        _      -> return b
resurrect b = return b
{-# INLINE resurrect #-}

contract :: Level -> MainNode k v -> MainNode k v
contract lev (CNode bmp arr) | lev > 0
                           , popCount bmp == 1
                           , SNode s <- A.head arr
                           = Tomb s
contract _ x = x
{-# INLINE contract #-}

-----------------------------------------------------------------------
-- * Lists

-- | /O(n * log n)/. Construct a map from a list of key/value pairs.
fromList :: (Eq k, Hashable k) => [(k,v)] -> IO (Map k v)
fromList xs = empty >>= \m -> mapM_ (\(k,v) -> insert k v m) xs >> return m
{-# INLINABLE fromList #-}

-- | /O(n)/. Unsafely convert the map to a list of key/value pairs.
--
-- WARNING: 'unsafeToList' makes no atomicity guarantees. Concurrent
-- changes to the map will lead to inconsistent results.
unsafeToList :: Map k v -> IO [(k,v)]
unsafeToList (Map root) = go root
    where
        go inode = do
            main <- readIORef inode
            case main of
                CNode bmp arr -> A.foldM' go2 [] (popCount bmp) arr
                Tomb (S k v) -> return [(k,v)]
                Collision xs -> return $ map (\(S k v) -> (k,v)) xs

        go2 xs (INode inode) = go inode >>= \ys -> return (ys ++ xs)
        go2 xs (SNode (S k v)) = return $ (k,v) : xs
{-# INLINABLE unsafeToList #-}

foldlWithKey :: Monad m => (forall x . IO x -> m x) ->
                (a -> k -> v -> m a) -> a -> Map k v -> m a
foldlWithKey liftio f !a0 (Map root) = go root a0
    where
        go inode a = do
            main <- liftio $ readIORef inode
            case main of
                CNode bmp arr -> A.foldM go2 a (popCount bmp) arr
                Tomb (S k v) -> f a k v
                Collision xs -> foldM (\a' (S k v) -> f a' k v) a xs

        go2 a (INode inode) = go inode a
        go2 a (SNode (S k v)) = f a k v


-----------------------------------------------------------------------

whenM :: Monad m => m Bool -> m () -> m ()
whenM p s = p >>= \t -> if t then s else return ()
{-# INLINE whenM #-}

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = p >>= \t -> if t then return () else s
{-# INLINE unlessM #-}

-----------------------------------------------------------------------

hashLength :: Int
hashLength = finiteBitSize (undefined :: Word)

bitsPerSubkey :: Int
bitsPerSubkey = floor . logBase (2 :: Float) . fromIntegral $ hashLength

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

index :: Hash -> Level -> Int
index h lev = fromIntegral $ (h `unsafeShiftR` lev) .&. subkeyMask
{-# INLINE index #-}

-- when or-ed with a CNode bitmap, determines if the hash is present
-- in the array at the given level of the trie
mask :: Hash -> Level -> Bitmap
mask h lev = 1 `unsafeShiftL` index h lev
{-# INLINE mask #-}

-- position in the CNode array
sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex bmp m = popCount ((m - 1) .&. bmp)
{-# INLINE sparseIndex #-}

nextLevel :: Level -> Level
nextLevel = (+) bitsPerSubkey
{-# INLINE nextLevel #-}

prevLevel :: Level -> Level
prevLevel = subtract bitsPerSubkey
{-# INLINE prevLevel #-}

-----------------------------------------------------------------------

-- TODO
--printMap :: (Show k, Show v) => Map k v -> IO ()
--printMap (Map root) = goI root
--    where
--        goI inode = putStr "(I " >> readIORef inode >>= goM >> putStr ")\n"
--        goM (CNode bmp arr) = do
--            putStr $ "(C " ++ (show bmp) ++ " ["
--            A.mapM_ (\b -> goB b >> putStr ", ") (popCount bmp) arr
--            putStr $ "] )"
--        goM (Tomb (S k v)) = putStr $ "(T " ++ (show k) ++ " " ++ (show v) ++ ")"
--        goM (Collision xs) = putStr $ "(Collision " ++ show xs ++ ")"
--        goB (INode i) = putStr "\n" >> goI i
--        goB (SNode (S k v)) = putStr $ "(" ++ (show k) ++ "," ++ (show v) ++ ")"
