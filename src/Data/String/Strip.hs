{-# LANGUAGE OverloadedStrings #-}

module Data.String.Strip (strip, hola)  where

import Data.Char
import Turtle

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

hola :: IO()
hola = echo "Hola"
