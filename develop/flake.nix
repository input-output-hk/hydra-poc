{

  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {
    feedback.url = "github:NorfairKing/feedback";
    flake-parts.url = "github:hercules-ci/flake-parts";
    horizon-gen-nix.url = "git+https://gitlab.horizon-haskell.net/haskell/horizon-gen-nix?ref=refs/tags/0.11.0";
    horizon-shell.url = "git+https://gitlab.horizon-haskell.net/shells/horizon-shell?ref=refs/tags/0.0.9";
  };

  outputs =
    inputs@
    { self
    , flake-parts
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; }
      {
        systems = [
          "x86_64-linux"
        ];
        perSystem = { system, ... }:
          {

            apps = {
              horizon-gen-nix = inputs.horizon-gen-nix.apps.${system}.default;
              horizon-shell = inputs.horizon-shell.apps.${system}.default;
            };

          };
      };
}
