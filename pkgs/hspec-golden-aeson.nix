{ mkDerivation, HUnit, QuickCheck, aeson, aeson-pretty, base
, bytestring, directory, filepath, hspec, hspec-core, lib
, quickcheck-arbitrary-adt, random, silently, transformers
}:
mkDerivation {
  pname = "hspec-golden-aeson";
  version = "0.9.0.0";
  sha256 = "04b1510fd21af7ccefbb753ffeb6474eed055120a27ef7fcbcc22a63bb0945d6";
  isLibrary = true;
  isExecutable = false;
  enableSeparateDataOutput = false;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring directory filepath hspec HUnit
    QuickCheck quickcheck-arbitrary-adt random transformers
  ];
  testHaskellDepends = [
    aeson base directory hspec hspec-core QuickCheck
    quickcheck-arbitrary-adt silently transformers
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  doHaddock = true;
  jailbreak = true;
  doCheck = false;
  doBenchmark = false;
  hyperlinkSource = false;
  homepage = "https://github.com/plow-technologies/hspec-golden-aeson#readme";
  description = "Use tests to monitor changes in Aeson serialization";
  license = lib.licenses.bsd3;
  broken = false;
}