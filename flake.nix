{
  description = "Neovim Register Manager";

  inputs.ribosome.url = git+https://git.tryp.io/tek/ribosome;

  outputs = { ribosome, ... }:
  let

    overrides = { self, source, buildInputs, pkgs, hsLib, ... }:
    let
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    in {
      uracil-test = inputs;
    };

  in ribosome.lib.flake ({ config, lib, ... }: {
    base = ./.;
    inherit overrides;
    compat.enable = false;
    devGhc.compiler = "ghc902";
    packages = {
      uracil = ./packages/uracil;
      uracil-test = ./packages/test;
    };
    main = "uracil-test";
    exe = "uracil";
    branch = "main";
    githubOrg = "tek";
    cachixName = "tek";
    cachixKey = "tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=";
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
  });
}
