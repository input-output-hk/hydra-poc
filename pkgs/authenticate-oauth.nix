{ mkDerivation, RSA, SHA, base, base64-bytestring, blaze-builder
, bytestring, crypto-pubkey-types, data-default, http-client
, http-types, lib, random, time, transformers, transformers-compat
}:
mkDerivation {
  pname = "authenticate-oauth";
  version = "1.7";
  sha256 = "746ff695fec1bd7c7b90f1952847ce3453fadf0f18a31db206753360b3219b78";
  revision = "1";
  editedCabalFile = "198xm2qdaqwg2m9kgrkw5gdk2bh19mmj6c4d5fsbpcjnhxlh6axg";
  isLibrary = true;
  isExecutable = false;
  enableSeparateDataOutput = false;
  libraryHaskellDepends = [
    base base64-bytestring blaze-builder bytestring crypto-pubkey-types
    data-default http-client http-types random RSA SHA time
    transformers transformers-compat
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  doHaddock = true;
  jailbreak = true;
  doCheck = false;
  doBenchmark = false;
  hyperlinkSource = false;
  homepage = "http://github.com/yesodweb/authenticate";
  description = "Library to authenticate with OAuth for Haskell web applications";
  license = lib.licenses.bsd3;
  broken = false;
}