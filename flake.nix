{
  description = "hydra-plutus example";

  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {

    flake-parts.url = "github:hercules-ci/flake-parts";

    horizon-cardano.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-cardano";

    lint-utils.url = "git+https://gitlab.nixica.dev/nix/lint-utils";

    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs =
    inputs@
    { self
    , flake-parts
    , horizon-cardano
    , lint-utils
    , nixpkgs
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; }
      {
        systems = [
          "aarch64-darwin"
          "x86_64-darwin"
          "x86_64-linux"
        ];
        perSystem = { system, pkgs, ... }:
          with pkgs.haskell.lib.compose;
          let
            myOverlay = final: prev: {
              hydra-cardano-api = doJailbreak (final.callCabal2nix "hydra-cardano-api" ./hydra-cardano-api { });
              hydra-cluster = doJailbreak (final.callCabal2nix "hydra-cluster" ./hydra-cluster { });
              hydra-node = doJailbreak (final.callCabal2nix "hydra-node" ./hydra-node { });
              hydra-prelude = doJailbreak (final.callCabal2nix "hydra-prelude" ./hydra-prelude { });
              hydra-plutus = doJailbreak (final.callCabal2nix "hydra-plutus" ./hydra-plutus { });
              hydra-plutus-extras = doJailbreak (final.callCabal2nix "hydra-plutus-extras" ./hydra-plutus-extras { });
              hydra-test-utils = doJailbreak (final.callCabal2nix "hydra-test-utils" ./hydra-test-utils { });
              hydra-tui = doJailbreak (final.callCabal2nix "hydra-tui" ./hydra-tui { });
            };

            legacyPackages = horizon-cardano.legacyPackages.${system}.extend (pkgs.lib.composeManyExtensions [ (import ./overlay.nix { inherit pkgs; }) myOverlay]);

          in
          {

      checks =
        with lint-utils.outputs.linters.${system}; {
          cabal-fmt = cabal-fmt { src = self; };
          fourmolu = fourmolu { src = self; opts = ""; };
          hlint = hlint { src = self; };
          nixpkgs-fmt = nixpkgs-fmt { src = self; };
        };

        devShells.default = legacyPackages.shellFor {
          packages = p: [
            p.hydra-cardano-api
            p.hydra-cluster
            p.hydra-node
            p.hydra-plutus
            p.hydra-plutus-extras
            p.hydra-prelude
            p.hydra-test-utils
            p.hydra-tui
          ];
          buildInputs = [
            legacyPackages.cabal-install
            pkgs.nixpkgs-fmt
          ];
        };

            packages = {
              inherit (legacyPackages)
                hydra-cardano-api
                hydra-cluster
                hydra-node
                hydra-prelude
                hydra-plutus
                hydra-plutus-extras
                hydra-test-utils
                hydra-tui
                ;
            };
          };

      };
}
