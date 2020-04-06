{ mkDerivation, base, containers, hspec, hspec-discover
, hspec-megaparsec, megaparsec, stdenv, text
}:
mkDerivation {
  pname = "masterarbeit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers megaparsec text ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec hspec-megaparsec megaparsec ];
  testToolDepends = [ hspec-discover ];
  description = "inductive coinductive dependent types interpreter";
  license = stdenv.lib.licenses.bsd3;
}
