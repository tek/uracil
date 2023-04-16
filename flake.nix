{
  description = "Neovim Register Manager";

  inputs = {
    ribosome.url = "git+https://git.tryp.io/tek/ribosome";
    hls.url = "github:haskell/haskell-language-server?ref=1.9.0.0";
  };

  outputs = {ribosome, hls, ...}: ribosome.lib.pro ({config, ...}: {
    compiler = "ghc925";
    depsFull = [ribosome];
    compat.enable = false;
    hackage.versionFile = "ops/version.nix";

    overrides = { self, source, buildInputs, pkgs, hsLib, ... }: {
      uracil = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    };

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Neovim";
        github = "tek/uracil";
        extra-source-files = ["readme.md" "changelog.md"];
      };
      ghc-options = ["-fplugin=Polysemy.Plugin"];
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = "^>= 0.5.1";
        };
        module = "Prelate";
      };
      dependencies = ["polysemy" "polysemy-plugin"];
    };

    packages.uracil = {
      src = ./packages/uracil;

      cabal.meta.synopsis = "Neovim Register Manager";

      library = {
        enable = true;
        dependencies = [
          "chiasma"
          "chronos"
          "exon"
          "extra"
          "polysemy-chronos"
          "ribosome"
          "ribosome-menu"
          "prettyprinter"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "chiasma"
          "polysemy-test"
          "ribosome"
          "ribosome-menu"
          "ribosome-test"
          "tasty"
          "uracil"
        ];
      };

      executable.enable = true;

    };

    envs.dev.buildInputs = with config.pkgs; [pkgs.neovim pkgs.tmux];
    envs.hls.hls.package = hls.packages.${config.system}.haskell-language-server-925;

    exe = "uracil";
    branch = "main";
    githubOrg = "tek";
    cachixName = "tek";
    cachixKey = "tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=";
  });
}
