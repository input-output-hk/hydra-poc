{
  description = "hydra-plutus example";

  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {

    flake-parts.url = "github:hercules-ci/flake-parts";

    horizon-plutus.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-plutus";

    lint-utils.url = "git+https://gitlab.nixica.dev/nix/lint-utils";

    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs =
    inputs@
    { self
    , flake-parts
    , horizon-plutus
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
              hspec-golden = final.callHackage "hspec-golden" "0.2.1.0" { };
              cardano-api = null;
              gitrev = final.callHackage "gitrev" "1.3.1" { };
              io-classes = final.callHackage "io-classes" "1.2.0.0" { };
              si-timers = final.callHackage "si-timers" "1.2.0.0" { };
              hydra-cardano-api = null;
              hydra-prelude = disableLibraryProfiling (final.callCabal2nix "hydra-prelude" ./hydra-prelude { });
              hydra-plutus = final.callCabal2nix "hydra-plutus" ./hydra-plutus { };
              hydra-plutus-extras = final.callCabal2nix "hydra-plutus-extras" ./hydra-plutus-extras { };
              hydra-test-utils = null;
              relude = doJailbreak (final.callHackage "relude" "1.2.1.0" { });
            };

            legacyPackages = horizon-plutus.legacyPackages.${system}.extend myOverlay;

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

            packages = { inherit (legacyPackages) hydra-prelude hydra-plutus hydra-plutus-extras; };
          };

      };
}
