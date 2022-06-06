module Uracil.Test.Run where

import Log (Severity (Debug))
import Polysemy.Test (UnitTest)
import Ribosome (HandlerError, HostConfig, PluginConfig (PluginConfig), setStderr)
import Ribosome.Test (StackWith, TestConfig (TestConfig), testEmbedConf, testHandler)

import Uracil.Plugin (UracilStack, interpretUracilStack)

type UraTestStack =
  Stop HandlerError : StackWith UracilStack

uraTestConf ::
  HostConfig ->
  Sem UraTestStack () ->
  UnitTest
uraTestConf conf =
  testEmbedConf @UracilStack (TestConfig False (PluginConfig "uracil" conf)) interpretUracilStack .
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
