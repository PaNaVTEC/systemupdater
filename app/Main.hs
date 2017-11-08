module Main where

import           Data.Aeson           (decode)
import           Data.ByteString.Lazy (readFile)
import           SystemUpdater.Data

main :: IO ()
main = do
  packages <- parsePackages
  install' packages

parsePackages ::  IO (Maybe Packages)
parsePackages = fmap decode (Data.ByteString.Lazy.readFile "./input/packages.json")

install' :: Maybe Packages -> IO()
install' (Just packages) = return () --install packages
install' Nothing         = return ()

install ::  Packages -> [()]
install (Packages ps) = fmap go ps
  where go ::  Package -> ()
        go (Package _ commands) = ()
