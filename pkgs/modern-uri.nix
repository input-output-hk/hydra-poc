{ mkDerivation, QuickCheck, base, bytestring, containers, criterion
, deepseq, exceptions, hashable, hspec, hspec-discover
, hspec-megaparsec, lib, megaparsec, mtl, profunctors, reflection
, tagged, template-haskell, text, weigh
}:
mkDerivation {
  pname = "modern-uri";
  version = "0.3.6.1";
  sha256 = "0246f9cfced1227abe3a6403eeacc6b5f79f7e3319759a4dd1fd341c12454fe9";
  revision = "1";
  editedCabalFile = "0c0mr8aqs963nmy7i8yfih24snaijgwkxim2q2khw12capshac0q";
  isLibrary = true;
  isExecutable = false;
  enableSeparateDataOutput = false;
  libraryHaskellDepends = [
    base bytestring containers deepseq exceptions hashable megaparsec
    mtl profunctors QuickCheck reflection tagged template-haskell text
  ];
  testHaskellDepends = [
    base bytestring hspec hspec-megaparsec megaparsec QuickCheck text
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    base bytestring criterion megaparsec text weigh
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  doHaddock = true;
  jailbreak = true;
  doCheck = false;
  doBenchmark = false;
  hyperlinkSource = false;
  homepage = "https://github.com/mrkkrp/modern-uri";
  description = "Modern library for working with URIs";
  license = lib.licenses.bsd3;
  broken = false;
}