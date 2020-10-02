{ mkDerivation, base, containers, hedgehog, hspec, hspec-discover
, hspec-expectations, hspec-hedgehog, hspec-megaparsec, megaparsec
, microlens-platform, mtl, optparse-applicative, parser-combinators
, prettyprinter, prettyprinter-ansi-terminal, repline, stdenv, text
}:
mkDerivation {
  pname = "masterarbeit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers megaparsec microlens-platform mtl
    parser-combinators prettyprinter prettyprinter-ansi-terminal text
  ];
  executableHaskellDepends = [
    base containers megaparsec microlens-platform mtl
    optparse-applicative prettyprinter prettyprinter-ansi-terminal
    repline text
  ];
  testHaskellDepends = [
    base containers hedgehog hspec hspec-expectations hspec-hedgehog
    hspec-megaparsec megaparsec microlens-platform mtl text
  ];
  testToolDepends = [ hspec-discover ];
  description = "inductive coinductive dependent types interpreter";
  license = stdenv.lib.licenses.bsd3;
}
