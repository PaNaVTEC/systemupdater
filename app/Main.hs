{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function
import           Data.List
import qualified Data.Map                  as Map
import           Data.Text                 (Text, pack, unpack)
import           Data.Tuple
import           Filesystem.Path.CurrentOS (encodeString)
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
  (sequence $ prependHome <$> ["go/bin", "go/src"]) >>= mktrees
  shell (pack $ "go get -u github.com/golang/lint/golint") empty
  return failPackages
  where
    packages = [YaourtPackage "go"]

prependHome :: String -> IO Turtle.FilePath
prependHome path = (\h -> h </> fromText (pack path)) <$> home

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

installGit :: IO ()
installGit = do
  failPackages <- toState "Git" . failedPackages <$> yin packages
  lnsfn (fromText "./config/git/gitconfig") (fromText "~/.gitconfig")
  --TODO: config gibo
  return ()
  where
    packages = [YaourtPackage "diff-so-fancy", YaourtPackage "gibo"]

lnsfn :: Turtle.FilePath -> Turtle.FilePath -> IO ExitCode
lnsfn s d = shell (pack $ "ln -sfn " ++ (encodeString s) ++  " " ++ (encodeString d)) empty

--installGit() {
--  # Global git ignores
--  gibo --upgrade
--  gibo Emacs Vim JetBrains Ensime Tags Vagrant Windows macOS Linux Archives >> ~/.gitignore.global
--  echo ".tern-project" >> ~/.gitignore.global
--}
