module Uracil.Test.Run where

import Log (Severity (Debug))
import Polysemy.Test (UnitTest)
import Ribosome (HandlerError, HostConfig, interpretPlugin, setStderr, testPlugin)
import Ribosome.Test (PluginStack, TestEffects, TestStack, runTestConf, testHandler)

import Uracil.Plugin (UracilStack, interpretUracilStack)

type UraTestStack =
  Stop HandlerError : TestEffects ++ UracilStack ++ PluginStack ++ TestStack

uraTestConf ::
  HostConfig ->
  Sem UraTestStack () ->
  UnitTest
uraTestConf conf =
  runTestConf False .
  interpretPlugin conf "test" mempty mempty .
  interpretUracilStack .
  testPlugin "test" mempty .
  testHandler

uraTest ::
  Sem UraTestStack () ->
  UnitTest
uraTest =
  uraTestConf def

uraTestDebug ::
  Sem UraTestStack () ->
  UnitTest
uraTestDebug =
  uraTestConf (setStderr Debug def)
