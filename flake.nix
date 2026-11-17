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
  };

  outputs = { self, haskell-nix, hackage-nix, kronor-haskell-packages, nixpkgs }:
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
        in
        {
          inherit pkgs project sqiff;
        };

      bySystem = forAllSystems perSystem;
    in
    {
      packages = forAllSystems (system: rec {
        sqiff = bySystem.${system}.sqiff;
        default = sqiff;
      });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${bySystem.${system}.sqiff}/bin/sqiff";
        };
      });

      devShells = forAllSystems (system:
        let inherit (bySystem.${system}) pkgs project; in {
          default = project.shellFor {
            tools.cabal = { version = "3.16.1.0"; };
            buildInputs = [ pkgs.postgresql_18 pkgs.sqitchPg pkgs.git ];
          };
        });
    };
}
