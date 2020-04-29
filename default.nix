{ mkDerivation, base, mtl, QuickCheck, stdenv, test-framework }:
mkDerivation {
  pname = "checked-exceptions";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base mtl ];
  testHaskellDepends = [ base mtl QuickCheck test-framework ];
  license = stdenv.lib.licenses.mpl20;
}
