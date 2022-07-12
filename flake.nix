{
  description = "Neovim Register Manager";

  inputs = {
    ribosome.url = git+https://git.tryp.io/tek/ribosome?ref=polysemy;
  };

  outputs = { ribosome, ... }:
  let
    overrides = { source, buildInputs, pkgs, ... }:
    let
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    in {
      uracil-test = inputs;
    };

  in ribosome.inputs.hix.lib.flake ({ config, lib, ... }: {
    base = ./.;
    inherit overrides;
    compat.enable = false;
    packages = {
      uracil = ./packages/uracil;
      uracil-test = ./packages/test;
    };
    main = "uracil-test";
    depsFull = [ribosome];
    hpack = {
      packages = import ./ops/hpack.nix { inherit config lib; };
      defaultApp = "uracil";
    };
    hackage.versionFile = "ops/version.nix";
    ghcid.shellConfig.buildInputs = with config.devGhc.pkgs; [pkgs.neovim pkgs.tmux];
    ghci = {
      preludePackage = "prelate";
      preludeModule = "Prelate";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedLabels"];
    };
    output.amend = project: outputs: {
      packages.static = project.pkgs.haskell.lib.justStaticExecutables outputs.packages.uracil;
    };
  });
}
