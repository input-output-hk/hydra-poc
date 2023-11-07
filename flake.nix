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
              hspec-golden-aeson = final.callHackage "hspec-golden-aeson" "0.9.0.0" { };
              hspec-golden = final.callHackage "hspec-golden" "0.2.1.0" { };
              hspec-junit-formatter = final.callHackage "hspec-junit-formatter" "1.1.0.2" { };
              hydra-cardano-api = doJailbreak (final.callCabal2nix "hydra-cardano-api" ./hydra-cardano-api { });
              hydra-cluster = doJailbreak (final.callCabal2nix "hydra-cluster" ./hydra-cluster { });
              hydra-node = doJailbreak (final.callCabal2nix "hydra-node" ./hydra-node { });
              hydra-prelude = doJailbreak (final.callCabal2nix "hydra-prelude" ./hydra-prelude { });
              hydra-plutus = doJailbreak (final.callCabal2nix "hydra-plutus" ./hydra-plutus { });
              hydra-plutus-extras = doJailbreak (final.callCabal2nix "hydra-plutus-extras" ./hydra-plutus-extras { });
              hydra-test-utils = doJailbreak (final.callCabal2nix "hydra-test-utils" ./hydra-test-utils { });
              hydra-tui = doJailbreak (final.callCabal2nix "hydra-tui" ./hydra-tui { });
              modern-uri = doJailbreak (final.callHackage "modern-uri" "0.3.6.1" { });
              quickcheck-dynamic = doJailbreak (final.callHackage "quickcheck-dynamic" "3.3.1" { });
              relude = doJailbreak (final.callHackage "relude" "1.2.1.0" { });
              req = doJailbreak (final.callHackage "req" "3.13.1" { });
              versions = doJailbreak (final.callHackage "versions" "6.0.3" { });
              wai-websockets = doJailbreak (final.callHackage "wai-websockets" "3.0.1.2" { });
            };

            legacyPackages = horizon-cardano.legacyPackages.${system}.extend myOverlay;

          in
          {

      checks =
        with lint-utils.outputs.linters.${system}; {
          cabal-fmt = cabal-fmt { src = self; };
          fourmolu = fourmolu { src = self; opts = ""; };
          hlint = hlint { src = self; };
          nixpkgs-fmt = nixpkgs-fmt { src = self; };
        };

            devShells.default = legacyPackages.hydra-prelude.env.overrideAttrs (attrs: {
              buildInputs = attrs.buildInputs ++ [
                legacyPackages.cabal-install
              ];
            });

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
