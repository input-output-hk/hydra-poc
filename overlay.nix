{ pkgs, ... }:

final: prev: with pkgs.haskell.lib; {
  RSA = final.callPackage (./pkgs/RSA.nix) { };

  authenticate-oauth = final.callPackage (./pkgs/authenticate-oauth.nix) { };

  crypto-pubkey-types = final.callPackage (./pkgs/crypto-pubkey-types.nix) { };

  hspec-golden = final.callPackage (./pkgs/hspec-golden.nix) { };

  hspec-golden-aeson = final.callPackage (./pkgs/hspec-golden-aeson.nix) { };

  hspec-junit-formatter = final.callPackage (./pkgs/hspec-junit-formatter.nix) { };

  iso8601-time = final.callPackage (./pkgs/iso8601-time.nix) { };

  modern-uri = final.callPackage (./pkgs/modern-uri.nix) { };

  quickcheck-arbitrary-adt = final.callPackage (./pkgs/quickcheck-arbitrary-adt.nix) { };

  quickcheck-dynamic = final.callPackage (./pkgs/quickcheck-dynamic.nix) { };

  relude = final.callPackage (./pkgs/relude.nix) { };

  req = final.callPackage (./pkgs/req.nix) { };

  retry = final.callPackage (./pkgs/retry.nix) { };

  versions = final.callPackage (./pkgs/versions.nix) { };

  wai-websockets = final.callPackage (./pkgs/wai-websockets.nix) { };

}
