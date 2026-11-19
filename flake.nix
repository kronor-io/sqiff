{
  description = "PostgreSQL schema diffs written as sqitch changes";

  inputs = {
    haskell-nix.url = "github:kronor-io/haskell.nix/nixpkgs-ghc-601a4d19";
    nixpkgs.url = "github:NixOS/nixpkgs/release-26.05";
    haskell-nix.inputs.hackage.follows = "hackage-nix";
    haskell-nix.inputs.nixpkgs.follows = "nixpkgs";
    hackage-nix.url = "github:kronor-io/hackage.nix";
    hackage-nix.flake = false;
    kronor-haskell-packages.url = "github:kronor-io/kronor-haskell-packages/repo";
    kronor-haskell-packages.flake = false;
    # The commit lint/cabal.project names.
    hopinion.url = "github:NorfairKing/hopinion/48eb343a003ab05f7df0c5844ba37897ab35f2e7";
    hopinion.flake = false;
  };

  outputs = { self, haskell-nix, hackage-nix, kronor-haskell-packages, nixpkgs, hopinion }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      perSystem = system:
        let
          haskellNix = import haskell-nix {
            inherit system;
            sourcesOverride.hackage = hackage-nix;
          };

          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (final: _: { postgresql = final.postgresql_18; })
              haskellNix.overlay
            ];
          };

          project = pkgs.haskell-nix.project {
            src = ./.;
            compiler-nix-name = "ghc9124";
            inputMap = {
              "https://kronor-io.github.io/kronor-haskell-packages" = kronor-haskell-packages;
            };
            modules = [{
              packages.sqiff.components.exes.sqiff.build-tools = [ pkgs.installShellFiles ];
              packages.sqiff.components.exes.sqiff.postInstall = ''
                $out/bin/sqiff --bash-completion-script $out/bin/sqiff > sqiff.bash
                $out/bin/sqiff --zsh-completion-script $out/bin/sqiff > sqiff.zsh
                $out/bin/sqiff --fish-completion-script $out/bin/sqiff > sqiff.fish
                installShellCompletion --cmd sqiff \
                  --bash sqiff.bash \
                  --zsh sqiff.zsh \
                  --fish sqiff.fish
              '';
            }];
          };

          sqiff = project.sqiff.components.exes.sqiff;

          # sqiff's hopinion rules (lint/). Built with sqiff's compiler, since
          # hopinion reads the .hie and .hi files that compiler writes.
          lintProject = pkgs.haskell-nix.cabalProject' {
            src = ./lint;
            compiler-nix-name = "ghc9124";
            # hopinion commits its .cabal files; without its package.yaml files
            # haskell.nix reads those rather than running hpack, which this
            # haskell.nix does not provide.
            inputMap = {
              "https://github.com/NorfairKing/hopinion/48eb343a003ab05f7df0c5844ba37897ab35f2e7" = pkgs.runCommand "hopinion-source" { } ''
                cp -r ${hopinion} $out
                chmod -R u+w $out
                find $out -name package.yaml -delete
              '';
            };
          };

          sqiff-lint = lintProject.hsPkgs.sqiff-lint.components.exes.sqiff-lint;
        in
        {
          inherit pkgs project sqiff sqiff-lint;
        };

      bySystem = forAllSystems perSystem;
    in
    {
      packages = forAllSystems (system: rec {
        sqiff = bySystem.${system}.sqiff;
        sqiff-lint = bySystem.${system}.sqiff-lint;
        default = sqiff;
      });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${bySystem.${system}.sqiff}/bin/sqiff";
        };
      });

      devShells = forAllSystems (system:
        let inherit (bySystem.${system}) pkgs project sqiff-lint; in {
          default = project.shellFor {
            tools.cabal = { version = "3.16.1.0"; };
            # scripts/check runs these.
            tools.fourmolu = { version = "0.19.0.1"; };
            tools.hlint = { version = "3.10"; };
            buildInputs = [ pkgs.postgresql_18 pkgs.sqitchPg pkgs.git sqiff-lint ];
          };
        });
    };
}
