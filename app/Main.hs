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

-- TODO: manage errors in a better way.
-- exitcode per package / command
main :: IO ()
main = do
  return ()

installBash :: IO PackageGroupExitState
installBash = toState "Bash" . failedPackages <$> yin packages
  where
    packages = [YaourtPackage "shunit2", YaourtPackage "shellcheck-static"]

installGo :: IO PackageGroupExitState
installGo = do
  failPackages <- toState "Go" . failedPackages <$> yin packages
  traverse (~/) ["go/bin", "go/src"] >>= mktrees
  return $ run "go get -u github.com/golang/lint/golint"
  return failPackages
  where
    packages = [YaourtPackage "go"]

installGit :: IO PackageGroupExitState
installGit = do
  failPackages <- toState "Git" . failedPackages <$> yin packages
  lnsfn (fromText "./config/git/gitconfig") (fromText "~/.gitconfig")
  createGitIgnore
  return failPackages
  where
    packages = [YaourtPackage "diff-so-fancy", YaourtPackage "gibo"]
    createGitIgnore = do
      return $ run "gibo --upgrade"
      content <- return $ run gibo
      path <- (~/) ".gitignore.global"
      createFileIfNotExists path content
      append path ".tern-project"
    gibo = "gibo Emacs Vim JetBrains Ensime Tags Vagrant Windows macOS Linux Archives"

yin :: [YaourtPackage] -> IO [PackageExitCode]
yin pkgs = zip pkgs <$> installGroup pkgs
  where
    installGroup pkgs = sequence $ installSingle <$> pkgs
    installSingle (YaourtPackage x) = shell (pack $ "yaourt -S --noconfirm " ++ x) empty

lnsfn :: Turtle.FilePath -> Turtle.FilePath -> IO ExitCode
lnsfn s d = shell (pack $ "ln -sfn " ++ (encodeString s) ++  " " ++ (encodeString d)) empty

(~/) :: MonadIO io => String -> io Turtle.FilePath
(~/) path = (\h -> h </> fromText (pack path)) <$> home

mktrees :: [Turtle.FilePath] -> IO ()
mktrees ps = mapM_ mktree ps

toState :: String -> [YaourtPackage] -> PackageGroupExitState
toState id []  = Success id
toState id pkg = Failure id pkg

failedPackages :: [PackageExitCode] -> [YaourtPackage]
failedPackages pkgs = fst <$> filter (\a -> snd a /= ExitSuccess) pkgs

createFileIfNotExists :: MonadIO io => Turtle.FilePath -> Shell Line -> io ()
createFileIfNotExists filepath line = do
  touch filepath
  output filepath line

run :: String -> Shell Line
run command = inshell (pack command) empty
