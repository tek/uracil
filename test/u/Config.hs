module Config where

import Prelude hiding (defaultTestConfig)
import Ribosome.Data.Setting (Setting(Setting))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))
import Ribosome.Test.Embed (TestConfig(tcVariables), Vars(..), varsFromList)
import qualified Ribosome.Test.Embed as E (defaultTestConfig, defaultTestConfigWith)

defaultVars :: IO Vars
defaultVars =
  return $ Vars def

defaultTestConfigWith :: Vars -> TestConfig
defaultTestConfigWith = E.defaultTestConfigWith "uracil"

defaultTestConfig :: TestConfig
defaultTestConfig = E.defaultTestConfig "uracil"

testConf :: (TestConfig -> TestConfig) -> TestConfig
testConf f =
  f defaultTestConfig

var ::
  MsgpackEncode a =>
  Text ->
  a ->
  TestConfig ->
  TestConfig
var name val conf =
  conf { tcVariables = tcVariables conf <> varsFromList [(name, toMsgpack val)] }

svar ::
  MsgpackEncode a =>
  Setting a ->
  a ->
  TestConfig ->
  TestConfig
svar (Setting name prefix _) =
  var (if prefix then "uracil_" <> name else name)
