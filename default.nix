with import <nixpkgs> { };

{ }:

stdenv.mkDerivation {
  name = "emacs.d";

  buildInputs = [ gnumake ];
}
