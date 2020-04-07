let
  compiler = "ghc882";
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              masterarbeit =
                haskellPackagesNew.callPackage ./masterarbeit.nix { };
            };
          };
        };
      };
    };
  };
  pkgs = import <nixos-unstable> { inherit config; };
in
  pkgs.haskell.packages."${compiler}".masterarbeit
