{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : $Header$
-- Description : Short description
-- Copyright   : (c) Mihai Maruseac
-- License     : BSD3
-- Maintainer  : mihai.maruseac@gmail.com
-- Stability   : stable
-- Portability : POSIX
--
-- Basic Types used by the library.
module BookmarkManager.Types
  (
    -- * Types
    Bookmark(..)
  , URI
  , Title
  , Tag
  , Description

    -- * Extra
  , testDocTest
  ) where

import Data.Text

-- | 'URI's are just 'Text' following the URI syntax (will be checked only
-- when creating the bookmark template).
type URI = Text

-- | A 'Title' is just a 'Text' value.
type Title = Text

-- | Tags are also 'Text' values but checked for typos (edit distance
-- suggestions).
type Tag = Text

-- | A 'Description' is also a 'Text' value on a single line.
type Description = Text

-- | Representation of a bookmark. Opaque to the user to allow some argument
-- checking.
data Bookmark = Bookmark
  { uri :: URI                    -- ^ Get the URI of this bookmark.
  , title :: Title                -- ^ Get the title of this bookmark.
  , tags :: [Tag]                 -- ^ Get the full list of tags of this
                                  --   bookmark.
  , descriptions :: [Description] -- ^ Extract the list of descriptions
                                  --   associated with the bookmark.
  }

-- |
-- Small test function to test the doctest support.
--
--  >>> let answer = 42 :: Int
--  >>> let prev = answer - 1
--  >>> testDocTest prev
--  42
--  >>> succ . Prelude.last . Prelude.take prev . iterate testDocTest $ 1
--  42
--
testDocTest :: Num a => a -> a
testDocTest x = x + 1
