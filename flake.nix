{
  description = "Neovim Register Manager";

  inputs = {
    ribosome.url = git+https://gitlab.tryp.io/haskell/ribosome?ref=polysemy;
    chiasma.url = github:tek/chiasma/main;
    polysemy-conc.url = github:tek/polysemy-conc;
    polysemy-time.url = github:tek/polysemy-time;
  };

  outputs = { ribosome, chiasma, polysemy-conc, polysemy-time, ... }:
  let
    inherit (ribosome.inputs) hix;

    overrides = { source, buildInputs, pkgs, ... }:
    let
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.rxvt-unicode];
    in {
      chiasma = source.package chiasma "chiasma";
      polysemy-conc = source.package polysemy-conc "conc";
      polysemy-process = source.package polysemy-conc "process";
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
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghcid.shellConfig.buildInputs = with config.devGhc.pkgs; [pkgs.neovim pkgs.tmux];
    ghci = {
      preludePackage = "incipit";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedLabels"];
    };
    output.amend = _: outputs: rec {
      apps = rec {
        uracil = {
          type = "app";
          program = "${outputs.packages.uracil}/bin/uracil";
        };
        default = uracil;
      };
    };
  });
}
