let H =
      https://gitlab.horizon-haskell.net/dhall/horizon-spec/-/raw/0.10.0/horizon-spec/package.dhall

let packages =
      { RSA = H.callHackage "RSA" "2.4.1"
      , authenticate-oauth = H.callHackage "authenticate-oauth" "1.7"
      , crypto-pubkey-types = H.callHackage "crypto-pubkey-types" "0.4.3"
      , hspec-golden = H.callHackage "hspec-golden" "0.2.1.0"
      , hspec-golden-aeson = H.callHackage "hspec-golden-aeson" "0.9.0.0"
      , hspec-junit-formatter = H.callHackage "hspec-junit-formatter" "1.1.0.2"
      , iso8601-time = H.callHackage "iso8601-time" "0.1.5"
      , modern-uri = H.callHackage "modern-uri" "0.3.6.1"
      , quickcheck-arbitrary-adt =
          H.callHackage "quickcheck-arbitrary-adt" "0.3.1.0"
      , quickcheck-dynamic = H.callHackage "quickcheck-dynamic" "3.3.1"
      , relude = H.callHackage "relude" "1.2.1.0"
      , retry = H.callHackage "retry" "0.9.3.1"
      , req = H.callHackage "req" "3.13.1"
      , versions = H.callHackage "versions" "6.0.3"
      , wai-websockets = H.callHackage "wai-websockets" "3.0.1.2"
      }

in  H.HorizonExport.MakeOverlay
      { overlayFile = "overlay.nix"
      , packagesDir = "pkgs/"
      , overlay = { compiler = "ghc-9.6.3", packages = toMap packages }
      }
