-- | The internal types giving the physical representation of

module Control.Concurrent.Map.Internal
       ( Map(..), INode(..),  MainNode(..), Branch(..), SNode(..)
       , Bitmap, Hash, Level )
       where

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

-- | A map from keys @k@ to values @v@.
newtype Map k v = Map (INode k v)
  deriving (Eq)

type INode k v = IORef (MainNode k v)

data MainNode k v = CNode !Bitmap !(A.Array (Branch k v))
                  | Tomb !(SNode k v)
                  | Collision ![SNode k v]

data Branch k v = INode !(INode k v)
                | SNode !(SNode k v)

data SNode k v = S !k v
    deriving (Eq, Show)

type Bitmap = Word
type Hash   = Word
type Level  = Int
