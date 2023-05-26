{ nixpkgs ? <nixpkgs>, config ? { } }:

with (import nixpkgs config);

stdenv.mkDerivation {
  name = "onlywands-mod-0.1.0";
  src = ./.;

  buildInputs = [ zip ];

  installPhase = ''
    mkdir -p $out/onlywands
    cp -r init.lua token.lua settings.lua mod.xml bin files $out/onlywands
    cd $out
    zip -r onlywands.zip onlywands
    cd ..
    rm -r $out/onlywands
  '';
}
