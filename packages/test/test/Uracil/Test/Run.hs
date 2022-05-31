module Uracil.Test.Run where

import Log (Severity (Debug))
import Polysemy.Test (UnitTest)
import Ribosome.Embed (PluginStack, TestEffects, interpretPlugin, testPlugin)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.HostConfig (HostConfig, log, logLevelStderr)
import Ribosome.Host.Test.Run (TestStack, runTestConf)
import Ribosome.Test.Error (testHandler)

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
  uraTestConf def { log = def { logLevelStderr = Debug} }
