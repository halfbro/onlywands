{ nixpkgs ? <nixpkgs>, config ? { } }:

with (import nixpkgs config);

let backendDrv = (import ./backend.nix { }).drv;

in stdenv.mkDerivation {
  name = "backend";
  src = ./.;
  installPhase = ''
    mkdir $out
    cp ${backendDrv}/bin/backend-app $out/backend-app
    mkdir $out/tokens
    mkdir $out/streamers
  '';
}
