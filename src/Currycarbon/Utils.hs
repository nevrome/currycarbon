module Currycarbon.Utils (
    CurrycarbonException (..),
) where

import           Control.Exception (Exception)

-- | Different exceptions for currycarbon
data CurrycarbonException =
    CurrycarbonCLIParsingException String
    deriving (Show)

instance Exception CurrycarbonException
