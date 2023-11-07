{ mkDerivation, base, directory, filepath, hspec, hspec-core
, hspec-discover, lib, optparse-applicative, silently
}:
mkDerivation {
  pname = "hspec-golden";
  version = "0.2.1.0";
  sha256 = "7e0cfe0c0427ada2f1947289641c59e5f66e50fc9e743ef52fb41c0b82e46e1d";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = false;
  libraryHaskellDepends = [ base directory filepath hspec-core ];
  executableHaskellDepends = [ base directory optparse-applicative ];
  testHaskellDepends = [ base directory hspec hspec-core silently ];
  testToolDepends = [ hspec-discover ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  doHaddock = true;
  jailbreak = true;
  doCheck = false;
  doBenchmark = false;
  hyperlinkSource = false;
  homepage = "https://github.com/stackbuilders/hspec-golden#readme";
  description = "Golden tests for hspec";
  license = lib.licenses.mit;
  broken = false;
}