{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = "0.1.0.0";
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2022 Torsten Schmits";
    category = "Neovim";
    build-type = "Simple";
    github = "tek/uracil";
  };

  base = {
    name = "base";
    version = ">= 4 && < 5";
    mixin = "hiding (Prelude)";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
    "-fplugin=Polysemy.Plugin"
  ];

  dependencies = [base "incipit" "polysemy" "polysemy-plugin"];

  basic = name: merge (meta // options) {
    inherit name;
    default-extensions = config.ghci.extensions;
  };

  project = name: basic name // {
    library = paths name // {
      source-dirs = "lib";
      inherit dependencies;
    };
  };

  exe = name: dir: merge (paths name // {
    main = "Main.hs";
    source-dirs = dir;
    inherit dependencies;
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  uracil = merge (project "uracil") {
    synopsis = "Neovim Register Manager";
    description = "See https://hackage.haskell.org/package/uracil/docs/Uracil.html";
    library.dependencies = [
      "chiasma"
      "chronos"
      "exon"
      "extra"
      "generic-lens"
      "lens"
      "polysemy-chronos"
      "ribosome"
      "ribosome-host"
      "ribosome-menu"
      "prettyprinter"
    ];
    executables.uracil = exe "uracil" "app" {
      dependencies = ["uracil"];
    };
  };

  uracil-test = merge (project "uracil-test") {
    synopsis = "Uracil tests";
    description = "See https://hackage.haskell.org/package/uracil/docs/Uracil.html";
    tests.uracil-test-unit = exe "uracil-test" "test" {
      dependencies = [
        "chiasma"
        "lens"
        "polysemy-test"
        "ribosome"
        "ribosome-host"
        "ribosome-host-test"
        "ribosome-test"
        "tasty"
        "uracil"
      ];
    };
  };

}
