{
  description = "Neovim Register Manager";

  inputs = {
    ribosome.url = git+https://gitlab.tryp.io/haskell/ribosome?ref=polysemy;
    polysemy-time.url = github:tek/polysemy-time;
  };

  outputs = { ribosome, polysemy-time, ... }:
  let
    inherit (ribosome.inputs) hix;

    overrides = { source, buildInputs, pkgs, ... }:
    let
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    in {
      polysemy-time = source.package polysemy-time "time";
      uracil-test = inputs;
    };

  in hix.lib.flake ({ config, lib, ... }: {
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
      preludePackage = "incipit";
      preludeModule = "Incipit";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedLabels"];
    };
  });
}
