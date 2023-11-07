{ mkDerivation, base, conduit, containers, directory, exceptions
, filepath, hspec, hspec-core, iso8601-time, lib, markdown-unlit
, temporary, text, time, xml-conduit, xml-types
}:
mkDerivation {
  pname = "hspec-junit-formatter";
  version = "1.1.0.2";
  sha256 = "2f002c6731e877cc9b044c975905c6ad4eeacbcb1432daa37e849f287574efbf";
  isLibrary = true;
  isExecutable = false;
  enableSeparateDataOutput = false;
  libraryHaskellDepends = [
    base conduit containers directory exceptions filepath hspec-core
    iso8601-time text time xml-conduit xml-types
  ];
  testHaskellDepends = [
    base containers filepath hspec markdown-unlit temporary text
    xml-conduit
  ];
  testToolDepends = [ markdown-unlit ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  doHaddock = true;
  jailbreak = true;
  doCheck = false;
  doBenchmark = false;
  hyperlinkSource = false;
  homepage = "https://github.com/freckle/hspec-junit-formatter#readme";
  description = "A JUnit XML runner/formatter for hspec";
  license = lib.licenses.mit;
  broken = false;
}