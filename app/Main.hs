{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Function
import           Data.List
import qualified Data.Map               as Map
import           Data.Text              (pack)
import           Data.Tuple
import           SystemUpdater.Data
import           Turtle

data PackageGroup = PackageGroup {id    :: String, state :: State}
data State = Success | Failure [YaourtPackage]

main :: IO ()
main = do
  return ()

-- refactorizar este codigo para extraer la logica relacionada
-- con el mapping de exitcodes a packagroup y razonar si esta logica no deberia
-- de estar en `yin`
bash :: IO PackageGroup
bash = toPackageGroup <$> fmap snd <$> Map.toList <$> snd <$> Map.partitionWithKey (\k _ -> k == ExitSuccess) <$> Map.fromList <$> fmap swap <$> yin packages
  where
    toPackageGroup []  = PackageGroup id Success
    toPackageGroup pkg = PackageGroup id (Failure pkg)
    id = "Bash"
    packages = [YaourtPackage "shunit2", YaourtPackage "shellcheck-static"]

yin :: [YaourtPackage] -> IO [(YaourtPackage, ExitCode)]
yin pkgs = zip pkgs <$> installGroup pkgs
  where
    installGroup pkgs = sequence $ installSingle <$> pkgs
    installSingle (YaourtPackage x) = shell (pack $ "yaourt -S --noconfirm " ++ x) empty

--  installGo() {
--  yaourt --noconfirm -S go
--  mkdir -p "$HOME/go/{bin,src}"
--  go get -u github.com/golang/lint/golint
--}
