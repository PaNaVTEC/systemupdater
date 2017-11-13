{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Function
import           Data.List
import qualified Data.Map               as Map
import           Data.Text              (pack)
import           SystemUpdater.Data
import           Turtle


data PackageGroup = PackageGroup {
                      id    :: String,
                      state :: State}
data State = Success | Failure [YaourtPackage]

main :: IO ()
main = do
  return ()

bash :: IO PackageGroup
bash = toPackageGroup <$> failurePackages
  where
    toPackageGroup []  = PackageGroup id Success
    toPackageGroup pkg = PackageGroup id (Failure pkg)
    failurePackages = fmap snd <$> Map.toList <$> snd <$> Map.partitionWithKey (\k _ -> k == ExitSuccess) <$> Map.fromList <$> processedPackages
    processedPackages :: IO [(ExitCode, YaourtPackage)]
    processedPackages = flip zip packages <$> yin packages
    id = "Bash"
    packages = [YaourtPackage "shunit2", YaourtPackage "shellcheck-static"]

yin :: [YaourtPackage] -> IO [ExitCode]
yin = sequence . fmap installSingle
  where installSingle (YaourtPackage x) = shell (pack $ "yaourt -S --noconfirm " ++ x) empty

--  installGo() {
--  yaourt --noconfirm -S go
--  mkdir -p "$HOME/go/{bin,src}"
--  go get -u github.com/golang/lint/golint
--}
