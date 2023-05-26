{ nixpkgs ? <nixpkgs>, config ? { } }:

with (import nixpkgs config);

let
  backend = import ./app/default.nix { };
  frontend = import ./ui/default.nix { };
  mod = import ./mod/default.nix { };

  drv = stdenv.mkDerivation {
    name = "onlywands-0.1.0";
    src = ./.;
    installPhase = ''
      mkdir -p $out/
      cp -r ${backend}/* $out/
      cp -r ${frontend}/* $out/
    '';
  };

  container = dockerTools.buildImage {
    name = "onlywands-dockerized";
    tag = "latest";

    copyToRoot = buildEnv {
      name = "image-root";
      paths = [ drv cacert ];
    };

    config = {
      Cmd = [ "/backend-app" ];
      WorkingDir = "/";
      ExposedPorts = { "8080/tcp" = { }; };
      Env = [
        "TWITCH_API_CLIENT_ID=${builtins.getEnv "TWITCH_API_CLIENT_ID"}"
        "TWITCH_API_CLIENT_SECRET=${builtins.getEnv "TWITCH_API_CLIENT_SECRET"}"
      ];
      Volumes = {
        "/streamers" = { };
        "/tokens" = { };
      };
    };
  };

in { inherit drv container mod; }
