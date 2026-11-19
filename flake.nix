{
  description = "PostgreSQL schema diffs written as sqitch changes";

  # What CI builds is pushed here (.github/workflows/check.yml).
  nixConfig = {
    extra-substituters = "https://kronor-open.cachix.org";
    extra-trusted-public-keys = "kronor-open.cachix.org-1:D1shHZh5BRkmM8RB9BaEqBURIgD/n5+u8KFXD1+DbF8=";
  };

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

          # sqiff with what it runs: sqitch, and psql under it, for the sqitch
          # commands, and git for sqiff table. sqiff baseline drives docker
          # compose, which has no place inside a container, so it is left out.
          dockerImage = pkgs.dockerTools.buildLayeredImage {
            name = "sqiff";
            tag = "latest";
            contents = [
              sqiff
              pkgs.sqitchPg
              pkgs.postgresql_18
              pkgs.git
              pkgs.cacert
              pkgs.dockerTools.fakeNss
            ];
            extraCommands = ''
              mkdir -p work tmp
              chmod 1777 tmp
            '';
            config = {
              Entrypoint = [ "/bin/sqiff" ];
              WorkingDir = "/work";
              Env = [ "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];
            };
          };
        in
        {
          inherit pkgs project sqiff sqiff-lint dockerImage;
        };

      # What CI builds on each system, as a GitHub Actions matrix, the way
      # kronor's other public packages do it: one job per package and system.
      githubPlatforms = {
        "x86_64-linux" = "ubuntu-latest";
        "aarch64-linux" = "ubuntu-24.04-arm";
        "aarch64-darwin" = "macos-latest";
      };

      bySystem = forAllSystems perSystem;
    in
    {
      packages = forAllSystems (system: rec {
        sqiff = bySystem.${system}.sqiff;
        sqiff-lint = bySystem.${system}.sqiff-lint;
        default = sqiff;
      } // nixpkgs.lib.optionalAttrs (nixpkgs.lib.hasSuffix "linux" system) {
        dockerImage = bySystem.${system}.dockerImage;
      });

      githubActions.matrix.include = nixpkgs.lib.concatMap
        (system: map
          (name: {
            inherit name system;
            os = githubPlatforms.${system};
            attr = "packages.${system}.${name}";
          })
          (builtins.attrNames (builtins.removeAttrs self.packages.${system}
            # sqiff-lint only runs in CI's check job, which is on aarch64-linux.
            ([ "default" ] ++ nixpkgs.lib.optional (system != "aarch64-linux") "sqiff-lint"))))
        systems;

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
