{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}

module SystemUpdater.Data where

import           Data.Aeson
import           GHC.Generics

newtype Packages = Packages [Package] deriving (Show, Generic, FromJSON)
data Package = Package {id :: String, commands :: [String]} deriving (Show, Generic, FromJSON)
