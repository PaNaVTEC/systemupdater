{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}

module SystemUpdater.Data where

import           Data.Aeson
import           GHC.Generics

type Packages = [Package] deriving (Show, Generic, FromJSON, Functor)
data Package = Package {id :: String, commands :: [String]} deriving (Show, Generic, FromJSON)
