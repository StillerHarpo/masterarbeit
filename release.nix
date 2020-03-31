let
  pkgs = import <nixos> { };
in
  pkgs.haskellPackages.callPackage ./masterarbeit.nix { }
