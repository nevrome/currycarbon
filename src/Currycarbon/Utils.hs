module Currycarbon.Utils (
    CurrycarbonException (..),
) where

import           Control.Exception (Exception)

data CurrycarbonException =
    CurrycarbonCLIParsingException String
    deriving (Show)

instance Exception CurrycarbonException
