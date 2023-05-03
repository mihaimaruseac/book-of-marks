-- | Example of a library file. It is also used for testing the test suites.
module BookmarkManager.Types
  (
    -- * Types
    Bookmark(..)
  , URI
  , Title
  , Tag
  , Description

    -- * Extra, exported functions
  , inc
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

-- | Increment one 'Num' value.
--
--  >>> let answer = 42 :: Int
--  >>> let prev = answer - 1
--  >>> inc prev
--  42
--  >>> succ . Prelude.last . Prelude.take prev . iterate inc $ 1
--  42
--
--  Properties (disabled now):
--  TODO(mihaimaruseac): Re-enable testing properties
--
--  prp> succ x == inc x
--  prp> inc (negate x) == negate (pred x)
--
inc :: Num a => a -- ^ value to increment
             -> a -- ^ result
inc x = x + 1
