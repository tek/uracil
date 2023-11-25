{
  description = "Neovim Register Manager";

  inputs = {
    ribosome.url = "git+https://git.tryp.io/tek/ribosome";
  };

  outputs = {ribosome, ...}: ribosome.lib.pro ({config, ...}: {
    depsFull = [ribosome];
    compat.enable = false;
    hackage.versionFile = "ops/version.nix";

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
          version = ">= 0.6 && < 0.8";
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
          "random"
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
        ];
      };

      executable.enable = true;

      buildInputs = [config.pkgs.neovim config.pkgs.tmux config.pkgs.xterm];

    };

    envs.dev.buildInputs = with config.pkgs; [pkgs.neovim pkgs.tmux];

    exe = "uracil";
    branch = "main";
    githubOrg = "tek";
    cachixName = "tek";
    cachixKey = "tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=";
  });
}
