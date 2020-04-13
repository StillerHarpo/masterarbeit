{ mkDerivation, base, containers, hspec, hspec-discover
, hspec-megaparsec, megaparsec, mtl, parser-combinators, stdenv
, text
}:
mkDerivation {
  pname = "masterarbeit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers megaparsec mtl parser-combinators text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers hspec hspec-megaparsec megaparsec mtl
  ];
  testToolDepends = [ hspec-discover ];
  description = "inductive coinductive dependent types interpreter";
  license = stdenv.lib.licenses.bsd3;
}
