{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, opaleye, stdenv, transformers }:
      mkDerivation {
        pname = "opaleye-tf";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base opaleye transformers ];
        homepage = "https://github.com/ocharles/opaleye-tf";
        description = "A client library for Opaleye using type families";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
