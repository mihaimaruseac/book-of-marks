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
    Bookmark
  , URI
  , Title
  , Tag
  , Description
  , ErrorReason

    -- * Constructing a 'Bookmark'
  , buildBookmark

    -- * Extraction functions
  , uri
  , title
  , tags
  , descriptions

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
--
-- Construct it using 'buildBookmark'.
--
-- Deconstruct using 'uri', 'title', 'tags' and 'descriptions'.
data Bookmark = Bookmark
  { uri :: URI                    -- ^ Get the URI of this bookmark.
  , title :: Title                -- ^ Get the title of this bookmark.
  , tags :: [Tag]                 -- ^ Get the full list of tags of this
                                  --   bookmark.
  , descriptions :: [Description] -- ^ Extract the list of descriptions
                                  --   associated with the bookmark.
  }

-- | The error reason.
type ErrorReason = Text

-- | The only way to build a bookmark
-- TODO: expand
-- TODO: implement
-- TODO: examples
buildBookmark :: URI -> Title -> [Tag] -> [Description] ->
  Either ErrorReason Bookmark
buildBookmark u ti ta d = Right $ Bookmark u ti ta d

-- |
-- Small test function to test the doctest support.
--
--  >>> let answer = 42 :: Int
--  >>> let prev = answer - 1
--  >>> testDocTest prev
--  42
--  >>> succ . last . take prev . iterate testDocTest $ 1
--  42
--
testDocTest :: Num a => a -> a
testDocTest x = x + 1
