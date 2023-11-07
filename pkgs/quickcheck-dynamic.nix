{ mkDerivation, QuickCheck, base, containers, lib, mtl, random, stm
, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "quickcheck-dynamic";
  version = "3.3.1";
  sha256 = "f435b0f1eaf6c5a8f917b945fa538013aee88a778497c7da741e5aaf00d201f6";
  isLibrary = true;
  isExecutable = false;
  enableSeparateDataOutput = false;
  libraryHaskellDepends = [ base containers mtl QuickCheck random ];
  testHaskellDepends = [
    base containers mtl QuickCheck stm tasty tasty-quickcheck
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  doHaddock = true;
  jailbreak = true;
  doCheck = false;
  doBenchmark = false;
  hyperlinkSource = false;
  homepage = "https://github.com/input-output-hk/quickcheck-dynamic#readme";
  description = "A library for stateful property-based testing";
  license = lib.licenses.asl20;
  broken = false;
}