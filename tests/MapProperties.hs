{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Hashable
import Data.IORef
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Concurrent.Map as CM
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- most of this based on the unordered-containers tests

-----------------------------------------------------------------------

main = defaultMain [ testGroup "basic interface"
                        [ testProperty "lookup" pLookup
                        , testProperty "insert" pInsert
                        , testProperty "delete" pDelete
                        , testProperty "insertIfAbsent" pInsertIfAbsent
                        ]
                     , testGroup "conversions"
                        [ testProperty "fromList" pFromList
                        , testProperty "unsafeToList" pUnsafeToList
                        , testProperty "unsafeToList2" pUnsafeToList2
                        ]
                     ]

-----------------------------------------------------------------------

type Model k v = M.Map k v

eq :: (Eq a, Eq k, Hashable k, Ord k)
   => (Model k v -> a) -> (CM.Map k v -> IO a) -> [(k, v)] -> Property
eq f g xs = monadicIO $ do
    let a = f (M.fromList xs)
    b <- run $ g =<< CM.fromList xs
    assert $ a == b

eq_ :: (Eq k, Eq v, Hashable k, Ord k)
    => (Model k v -> Model k v) -> (CM.Map k v -> IO ()) -> [(k, v)] -> Property
eq_ f g xs = monadicIO $ do
    let a = M.toAscList $ f $ M.fromList xs
    m <- run $ CM.fromList xs
    run $ g m
    b <- run $ unsafeToAscList m
    assert $ a == b

unsafeToAscList :: Ord k => CM.Map k v -> IO [(k, v)]
unsafeToAscList m = do
    xs <- CM.unsafeToList m
    return $ L.sortBy (compare `on` fst) xs

unsafeToAscList2 :: Ord k => CM.Map k v -> IO [(k, v)]
unsafeToAscList2 m = do
    acc <- newIORef []
    let doelem k v   = modifyIORef acc ((k,v):)
        dosplit n fn = forM_ [0 .. n-1] fn
    CM.unsafeTreeTraverse m doelem dosplit
    xs <- readIORef acc
    return $ L.sortBy (compare `on` fst) xs

-----------------------------------------------------------------------

-- key type that generates more hash collisions

newtype Key = K { unK :: Int }
    deriving (Arbitrary, Eq, Ord)

instance Show Key where
    show = show . unK

instance Hashable Key where
    hashWithSalt salt k = hashWithSalt salt (unK k) `mod` 20

-----------------------------------------------------------------------

pLookup :: Key -> [(Key,Int)] -> Property
pLookup k = M.lookup k `eq` CM.lookup k

pInsert :: Key -> Int -> [(Key,Int)] -> Property
pInsert k v = M.insert k v `eq_` CM.insert k v

pDelete :: Key -> [(Key,Int)] -> Property
pDelete k = M.delete k `eq_` CM.delete k

pInsertIfAbsent :: Key -> Int -> [(Key,Int)] -> Property
pInsertIfAbsent k v = (M.insertWith (\new old -> old) k v)
                      `eq_` (\m -> do
                                _ <- CM.insertIfAbsent k v m
                                return ())

pFromList :: [(Key,Int)] -> Property
pFromList = id `eq_` (\_ -> return ())

pUnsafeToList :: [(Key,Int)] -> Property
pUnsafeToList = M.toAscList `eq` unsafeToAscList

pUnsafeToList2 :: [(Key,Int)] -> Property
pUnsafeToList2 = M.toAscList `eq` unsafeToAscList2
