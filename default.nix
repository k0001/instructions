{ mkDerivation, base, free, stdenv }:
mkDerivation {
  pname = "instructions";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base free ];
  description = "Build DSLs and effectful interpreters using free monads";
  license = stdenv.lib.licenses.unfree;
}
