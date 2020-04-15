{ mkDerivation, base, containers, hspec, hspec-discover
, hspec-megaparsec, megaparsec, microlens-platform, mtl
, parser-combinators, stdenv, text
}:
mkDerivation {
  pname = "masterarbeit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers megaparsec microlens-platform mtl
    parser-combinators text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers hspec hspec-megaparsec megaparsec
    microlens-platform mtl text
  ];
  testToolDepends = [ hspec-discover ];
  description = "inductive coinductive dependent types interpreter";
  license = stdenv.lib.licenses.bsd3;
}
