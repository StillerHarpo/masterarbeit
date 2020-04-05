{ mkDerivation, base, containers, megaparsec, stdenv, text }:
mkDerivation {
  pname = "masterarbeit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers megaparsec text ];
  description = "inductive coinductive dependent types interpreter";
  license = stdenv.lib.licenses.bsd3;
}
