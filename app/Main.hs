{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Text              (pack)
import           SystemUpdater.Data
import           Turtle

main :: IO ()
main = do
  yaourt [YaourtPackage "shunit2", YaourtPackage "shellcheck-static"]
  return ()

yaourt :: [YaourtPackage] -> IO [ExitCode]
yaourt = sequence . fmap installSingle
  where installSingle (YaourtPackage x) = shell (pack $ "yaourt -S --noconfirm " ++ x) empty
