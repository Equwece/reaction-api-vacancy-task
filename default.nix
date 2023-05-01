let
  stableNixpkgs = import ./stableNixpkgs.nix;
  nixpkgs = import (stableNixpkgs) {};
in
  nixpkgs.haskellPackages.callPackage ./package.nix {}

