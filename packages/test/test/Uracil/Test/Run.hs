module Uracil.Test.Run where

import Log (Severity (Debug))
import Polysemy.Test (UnitTest)
import Ribosome.Embed (PluginStack, embedNvimPluginConf)
import Ribosome.Host (Rpc)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.HostConfig (log, logLevelStderr)
import Ribosome.Host.Test.Run (TestStack, runTestConf)
import Ribosome.Test.Error (testHandler)

import Uracil.Plugin (UracilStack, interpretUracilStack)

type UraTestStack =
  Stop HandlerError : Rpc : PluginStack ++ UracilStack ++ TestStack

uraTest ::
  Sem UraTestStack () ->
  UnitTest
uraTest =
  runTestConf False .
  interpretUracilStack .
  embedNvimPluginConf def "test" mempty mempty mempty .
  testHandler

uraTestDebug ::
  Sem UraTestStack () ->
  UnitTest
uraTestDebug =
  runTestConf False .
  interpretUracilStack .
  embedNvimPluginConf def { log = def { logLevelStderr = Debug} } "test" mempty mempty mempty .
  testHandler
