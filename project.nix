{ mkDerivation, base, mtl, stdenv, text, text-render }:
mkDerivation {
  pname = "error-list";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base mtl text text-render ];
  homepage = "http://github.com/thinkpad20/error-list";
  description = "A useful type for collecting error messages";
  license = stdenv.lib.licenses.mit;
}
