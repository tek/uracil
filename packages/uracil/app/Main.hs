import Neovim (neovim, defaultConfig, plugins)

import Uracil.Plugin (plugin)

main :: IO ()
main =
  neovim defaultConfig { plugins = [plugin] }
