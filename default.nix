{ mkDerivation, base, bytestring, opaleye, postgresql-simple
, product-profunctors, profunctors, stdenv, tasty, tasty-hunit
, text, time, transformers
}:
mkDerivation {
  pname = "opaleye-tf";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring opaleye postgresql-simple product-profunctors
    profunctors text time transformers
  ];
  testHaskellDepends = [ base tasty tasty-hunit ];
  homepage = "https://github.com/ocharles/opaleye-tf";
  description = "A client library for Opaleye using type families";
  license = stdenv.lib.licenses.bsd3;
}
