{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function
import           Data.List
import qualified Data.Map               as Map
import           Data.Text              (pack, unpack)
import           Data.Tuple
import           SystemUpdater.Data
import           Turtle

data PackageGroupExitState = Success String | Failure String [YaourtPackage]
type PackageExitCode = (YaourtPackage, ExitCode)

main :: IO ()
main = do
  return ()

bash :: IO PackageGroupExitState
bash = toState "Bash" . failedPackages <$> yin packages
  where
    packages = [YaourtPackage "shunit2", YaourtPackage "shellcheck-static"]

installGo :: IO PackageGroupExitState
installGo = do
  failPackages <- toState "Go" . failedPackages <$> yin packages
  (\h -> h </> "go/bin") <$> home >>= mktree
  return failPackages
  where
    packages = [YaourtPackage "go"]

mktrees :: [Turtle.FilePath] -> IO ()
mktrees ps = mapM_ mktree ps

toState :: String -> [YaourtPackage] -> PackageGroupExitState
toState id []  = Success id
toState id pkg = Failure id pkg

failedPackages :: [PackageExitCode] -> [YaourtPackage]
failedPackages pkgs = fst <$> filter (\a -> snd a /= ExitSuccess) pkgs

yin :: [YaourtPackage] -> IO [PackageExitCode]
yin pkgs = zip pkgs <$> installGroup pkgs
  where
    installGroup pkgs = sequence $ installSingle <$> pkgs
    installSingle (YaourtPackage x) = shell (pack $ "yaourt -S --noconfirm " ++ x) empty

--  installGo() {
--  yaourt --noconfirm -S go
--  mkdir -p "$HOME/go/{bin,src}"
--  go get -u github.com/golang/lint/golint
--}
