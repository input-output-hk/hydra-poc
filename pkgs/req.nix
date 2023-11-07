{ mkDerivation, QuickCheck, aeson, authenticate-oauth, base
, blaze-builder, bytestring, case-insensitive, containers
, crypton-connection, exceptions, hspec, hspec-core, hspec-discover
, http-api-data, http-client, http-client-tls, http-types, lib
, modern-uri, monad-control, mtl, retry, template-haskell, text
, time, transformers, transformers-base, unliftio-core
}:
mkDerivation {
  pname = "req";
  version = "3.13.1";
  sha256 = "1045c4158ef361ad913acfcc628e6ddf0c3131c33a7abc83858ec0bfa55bf932";
  revision = "3";
  editedCabalFile = "0dib8rwavjrp2bsv3pl3bgblqdfvky53k7qnv7ca4sq8070aqffp";
  isLibrary = true;
  isExecutable = false;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson authenticate-oauth base blaze-builder bytestring
    case-insensitive containers crypton-connection exceptions
    http-api-data http-client http-client-tls http-types modern-uri
    monad-control mtl retry template-haskell text transformers
    transformers-base unliftio-core
  ];
  testHaskellDepends = [
    aeson base blaze-builder bytestring case-insensitive hspec
    hspec-core http-api-data http-client http-types modern-uri
    monad-control mtl QuickCheck retry template-haskell text time
  ];
  testToolDepends = [ hspec-discover ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  doHaddock = true;
  jailbreak = true;
  doCheck = false;
  doBenchmark = false;
  hyperlinkSource = false;
  homepage = "https://github.com/mrkkrp/req";
  description = "HTTP client library";
  license = lib.licenses.bsd3;
  broken = false;
}