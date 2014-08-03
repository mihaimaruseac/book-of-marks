{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Data.Acid
import Data.SafeCopy
import Data.ByteString
import Data.Typeable (Typeable)

import Prelude hiding (getLine, putStr)

type URI = ByteString
type Tag = ByteString
type Description = ByteString
data Test = T URI [Tag] Description deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Test)

writeTest :: Test -> Update Test ()
writeTest = put

queryTest :: Query Test Test
queryTest = ask

$(makeAcidic ''Test ['writeTest, 'queryTest])

main = do
  st <- openLocalState $ T "about:blank" [] "Default homepage"
  dump st
  insert st
  dump st
  closeAcidState st

dump st = query st QueryTest >>= print
insert st = do
  putStr "Enter uri: "
  uri <- getLine
  putStr "Enter description: "
  description <- getLine
  update st . WriteTest $ T uri [] description
