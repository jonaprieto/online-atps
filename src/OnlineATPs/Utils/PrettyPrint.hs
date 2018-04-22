
-- | Utilities for pretty printing.
-- Adapted from @Apia.Utils.PrettyPrint@

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module OnlineATPs.Utils.PrettyPrint
  ( module Text.PrettyPrint
  , bquotes
  , Pretty ( pretty )
  , prettyShow
  , squotes
  ) where

import Text.PrettyPrint

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ( (<>) )
#endif

-- | Wrap a document in ‘...’.
bquotes ∷ Doc → Doc
bquotes d = char '‘' <> d <> char '’'

-- | Wrap a string in ‘...’.
squotes ∷ String → Doc
squotes = bquotes . text

-- | Use instead of 'show' when printing to world.
prettyShow :: Pretty a ⇒ a → String
prettyShow = render . pretty

-- | Pretty print type class.
class Pretty a where
  pretty ∷ a → Doc

instance Pretty Doc where
  pretty = id

instance Pretty String where
  pretty = text
