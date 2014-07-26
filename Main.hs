{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import Data.ByteString
import Data.Typeable (Typeable)

import Prelude hiding (getLine, putStr)

type URI = ByteString
type Tag = ByteString
type Description = ByteString
data Test = T URI [Tag] Description deriving (Show, Typeable)

instance SafeCopy Test where
  putCopy (T uri tags desc) = contain $ safePut uri >> safePut tags >> safePut desc
  getCopy = contain $ T <$> safeGet <*> safeGet <*> safeGet

writeTest :: Test -> Update Test ()
writeTest t = put t

queryTest :: Query Test Test
queryTest = ask

data WriteState = WriteState Test deriving (Typeable)
data QueryState = QueryState deriving (Typeable)

instance SafeCopy WriteState where
  putCopy (WriteState t) = putCopy t
  getCopy = contain $ liftM WriteState safeGet

instance SafeCopy QueryState where
  putCopy QueryState = contain $ return () -- nothing to save
  getCopy = contain $ return QueryState -- nothing to read

instance Method WriteState where
  type MethodResult WriteState = ()
  type MethodState WriteState = Test

-- type family
instance Method QueryState where
  type MethodResult QueryState = Test
  type MethodState QueryState = Test

-- type family
instance UpdateEvent WriteState
instance QueryEvent QueryState

instance IsAcidic Test where
  acidEvents =
    [ UpdateEvent (\(WriteState t) -> writeTest t)
    , QueryEvent (\QueryState -> queryTest)
    ]

main = do
  st <- openLocalState $ T "about:blank" [] "Default homepage"
  dump st
  insert st
  dump st
  closeAcidState st

insert :: AcidState Test -> IO ()
dump st = query st QueryState >>= print
insert st = do
  putStr "Enter uri: "
  uri <- getLine
  putStr "Enter description: "
  description <- getLine
  update st . WriteState $ T uri [] description
