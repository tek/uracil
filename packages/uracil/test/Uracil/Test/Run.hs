module Uracil.Test.Run where

import Log (Severity (Trace))
import Polysemy.Test (UnitTest)
import Ribosome (HostConfig, PluginConfig (PluginConfig), Report, setStderr)
import Ribosome.Test (EmbedStackWith, TestConfig (TestConfig), testEmbedConf, testHandler)

import Uracil.Plugin (UracilStack, interpretUracilStack)

type UraTestStack =
  Stop Report : EmbedStackWith UracilStack

testConfig ::
  HostConfig ->
  TestConfig
testConfig conf =
  TestConfig False (PluginConfig "uracil" conf unit)

testConfigDebug ::
  HostConfig ->
  TestConfig
testConfigDebug conf =
  testConfig (setStderr Trace conf)

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

uraTestTrace ::
  Sem UraTestStack () ->
  UnitTest
uraTestTrace =
  uraTestConf (setStderr Trace def)
