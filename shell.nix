with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "libjuice-hs";
  buildInputs = [ stack
                  gnumake
                  haskellPackages.c2hs
                ];
}
