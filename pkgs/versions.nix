{ mkDerivation, base, deepseq, hashable, lib, megaparsec, microlens
, parser-combinators, tasty, tasty-hunit, template-haskell, text
}:
mkDerivation {
  pname = "versions";
  version = "6.0.3";
  sha256 = "cf32dff95de0360b37a7a3488960d75995237644781c19fde4d083630bfdea4c";
  isLibrary = true;
  isExecutable = false;
  enableSeparateDataOutput = false;
  libraryHaskellDepends = [
    base deepseq hashable megaparsec parser-combinators
    template-haskell text
  ];
  testHaskellDepends = [
    base megaparsec microlens tasty tasty-hunit template-haskell text
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  doHaddock = true;
  jailbreak = true;
  doCheck = false;
  doBenchmark = false;
  hyperlinkSource = false;
  homepage = "https://github.com/fosskers/versions";
  description = "Types and parsers for software version numbers";
  license = lib.licenses.bsd3;
  broken = false;
}