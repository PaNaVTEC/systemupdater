{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base    (bracket)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Managed     (MonadManaged, managed_)
import           Data.Function
import           Data.List
import qualified Data.Map                  as Map
import           Data.Text                 (Text, pack, unpack)
import           Data.Tuple
import           Filesystem.Path.CurrentOS (encodeString)
import           System.Process            (proc)
import           SystemUpdater.Data
import           Turtle


data PackageGroupExitState = Success String | Failure String [YaourtPackage]
type PackageExitCode = (YaourtPackage, ExitCode)

-- TODO: Find a way to execute commands in the same shell like (cd ~ && mkdir hola)
-- TODO: manage errors in a better way.
-- exitcode per package / command
-- TODO specify dotfiles directory

withShell :: Turtle.FilePath -> IO r -> IO r
withShell path f = bracket (cd path) (\_ -> cd "/home/panavtec/Code/Github/systemupdater") (\_ -> f)

withPath :: Turtle.FilePath -> Managed ()
withPath path = managed_ (withShell path)

main :: IO ()
main = do
  foo
  dir <- pwd
  fileName <- return $ dir </> "EsteEsElCurrent"
  createFileIfNotExists fileName empty
  return ()

  where
    foo :: IO ()
    foo = do
      with (withPath "/home/panavtec") (\_ -> createFileInHome)
    createFileInHome :: IO ()
    createFileInHome = do
      dir <- pwd
      fileName <- return $ dir </> "EsteEsHome"
      createFileIfNotExists fileName empty
      return ()

--   home >>= cd
--   view pwd
--   newShell
--   view pwd
--   d <- single $ f
--   print d
--   return ()
--   where f = do
--           run' "cd ~" .&&. run' "pwd"

--installEmacs() {
--  (cd "$dir/config/emacs/install/" && makepkg -si)
--  mkdir -p "$HOME/.emacs.saves"
--  mkdir -p "$HOME/.emacs.undo"
--  touch "$HOME/.emacs.d/custom.el"
--
--  yaourt -S --noconfirm xsel # Fixes clipboard
--  git clone https://github.com/jdee-emacs/jdee-server.git ~/.jdee-server
--  (
--    cd ~/.jdee-server
--    mvn -Dmaven.test.skip=true package
--  )
--
--  ln -sfn "$dir/config/emacs/init.el" "$HOME/.emacs.d/"
--  ln -sfn "$dir/config/emacs/lisp/" "$HOME/.emacs.d/lisp"
--  ln -sfn "$dir/config/emacs/snippets/" "$HOME/.emacs.d/snippets"
--
--  mkdir -p "$HOME/.config/systemd/user"
--  ln -sfn "$dir/config/units/emacs.service" "$HOME/.config/systemd/user/emacs.service"
--  systemctl --user enable --now emacs
--}

--installEmacs :: IO PackageGroupExitState
--installEmacs = do
--
--  using (cd "./config/emacs/install/") >>=
--  return $ run "makepkg -si"

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
  lnsfn (filePath "./config/git/gitconfig") (filePath "~/.gitconfig")
  createGitIgnore
  return failPackages
  where
    packages = [YaourtPackage "diff-so-fancy", YaourtPackage "gibo"]
    createGitIgnore = do
      return $ run "gibo --upgrade"
      content <- return $ run gibo
      path <- (~/) ".gitignore.global"
      content &> path
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
(~/) path = (\h -> h </> filePath path ) <$> home

filePath :: String -> Turtle.FilePath
filePath p = fromText $ pack p

mktrees :: [Turtle.FilePath] -> IO ()
mktrees ps = mapM_ mktree ps

toState :: String -> [YaourtPackage] -> PackageGroupExitState
toState id []  = Success id
toState id pkg = Failure id pkg

failedPackages :: [PackageExitCode] -> [YaourtPackage]
failedPackages pkgs = fst <$> filter (\a -> snd a /= ExitSuccess) pkgs

(&>) :: MonadIO io => Shell Line -> Turtle.FilePath -> io ()
line &> filepath = do
  touch filepath
  output filepath line

createFileIfNotExists :: MonadIO io => Turtle.FilePath -> Shell Line -> io ()
createFileIfNotExists = flip (&>)

run :: String -> Shell Line
run command = inshell (pack command) empty

run' :: String -> Shell ExitCode
run' command = shell (pack command) empty

newShell ::MonadIO io => io ExitCode
newShell = Turtle.system (System.Process.proc "/bin/sh" []) empty
