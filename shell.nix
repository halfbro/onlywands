{ nixpkgs ? import <nixpkgs> { }, config ? { } }:

nixpkgs.mkShell {
  shellHook = ''
    source .env
  '';
}
