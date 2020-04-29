{ mkDerivation, base, QuickCheck, stdenv, test-framework }:
mkDerivation {
  pname = "checked-exceptions";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base QuickCheck test-framework ];
  license = stdenv.lib.licenses.mpl20;
}
