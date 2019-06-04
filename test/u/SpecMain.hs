{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Test.Framework.BlackBoxTest ()
import {-@ HTF_TESTS @-} DiagSpec
import {-@ HTF_TESTS @-} PasteSpec
import {-@ HTF_TESTS @-} YankMenuSpec
import {-@ HTF_TESTS @-} YankSpec

main :: IO ()
main = htfMain htf_importedTests
