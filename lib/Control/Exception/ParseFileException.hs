{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}

module Control.Exception.ParseFileException where

import Control.Exception
import Data.ByteString.Char8

data ParseFileException = PFPartial | PFFail ByteString [String] String
    deriving stock (Show)
    deriving anyclass (Exception)