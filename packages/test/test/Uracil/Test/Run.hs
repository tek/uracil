module Uracil.Test.Run where

import Log (Severity (Debug))
import Polysemy.Test (UnitTest)
import Ribosome (HandlerError, HostConfig, PluginConfig (PluginConfig), setStderr)
import Ribosome.Test (StackWith, TestConfig (TestConfig), testEmbedConf, testHandler)

import Uracil.Plugin (UracilStack, interpretUracilStack)

type UraTestStack =
  Stop HandlerError : StackWith UracilStack

testConfig ::
  HostConfig ->
  TestConfig
testConfig conf =
  TestConfig False (PluginConfig "uracil" conf)

uraTestConf ::
  HostConfig ->
  Sem UraTestStack () ->
  UnitTest
uraTestConf conf =
  testEmbedConf @UracilStack (testConfig conf) interpretUracilStack .
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
