{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Data.SafeCopy
import Data.ByteString

type URI = ByteString
type Tag = ByteString
type Description = ByteString
data Test = T URI [Tag] Description deriving Show

instance SafeCopy Test where
  putCopy (T uri tags desc) = contain $ safePut uri >> safePut tags >> safePut desc
  getCopy = contain $ T <$> safeGet <*> safeGet <*> safeGet

main = undefined
