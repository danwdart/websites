{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Control.Exception.InvalidDateException where

import Control.Exception

data InvalidDateException = InvalidDateException String
    deriving stock (Show)
    deriving anyclass (Exception)
