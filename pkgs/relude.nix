{ mkDerivation, Glob, base, bytestring, containers, deepseq
, doctest, ghc-prim, hashable, hedgehog, lib, mtl, stm, tasty-bench
, text, transformers, unordered-containers
}:
mkDerivation {
  pname = "relude";
  version = "1.2.1.0";
  sha256 = "784f745bfa6bc008189c8d2af62a59e1a466b429f85d6427f6e25bf38545d725";
  revision = "1";
  editedCabalFile = "1dqgl50rlrq0d84x5ffm5d4sp275wfm3f03k25ryq0ni1prhrarc";
  isLibrary = true;
  isExecutable = false;
  enableSeparateDataOutput = false;
  libraryHaskellDepends = [
    base bytestring containers deepseq ghc-prim hashable mtl stm text
    transformers unordered-containers
  ];
  testHaskellDepends = [
    base bytestring containers doctest Glob hedgehog text
  ];
  benchmarkHaskellDepends = [
    base tasty-bench unordered-containers
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  doHaddock = true;
  jailbreak = true;
  doCheck = false;
  doBenchmark = false;
  hyperlinkSource = false;
  homepage = "https://github.com/kowainik/relude";
  description = "Safe, performant, user-friendly and lightweight Haskell Standard Library";
  license = lib.licenses.mit;
  broken = false;
}