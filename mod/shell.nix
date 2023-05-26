{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell { packages = [ sumneko-lua-language-server ]; }
