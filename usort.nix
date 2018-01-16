{ mkDerivation, base, bytestring, formatting, haskeline, stdenv
, tasty, tasty-golden, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "usort";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base formatting haskeline text ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    base bytestring tasty tasty-golden tasty-hunit tasty-quickcheck
    text
  ];
  homepage = "http://github.com/chreekat/usort#readme";
  description = "With usort, the sort is you!";
  license = stdenv.lib.licenses.agpl3;
}
