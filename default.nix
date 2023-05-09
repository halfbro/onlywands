{ nixpkgs ? <nixpkgs>, config ? { } }:

with (import nixpkgs config);

let
  backend = import ./app/default.nix { };
  frontend = import ./ui/default.nix { };

  drv = stdenv.mkDerivation {
    name = "onlywands-0.1.0";
    src = ./.;
    installPhase = ''
      mkdir -p $out/
      ls -al ${backend}/
      cp -r ${backend}/* $out/
      cp -r ${frontend}/* $out/
    '';
  };

  container = dockerTools.buildImage {
    name = "onlywands-dockerized";
    tag = "latest";

    contents = [ drv cacert ];

    config = {
      Cmd = [ "${drv}/backend-app" ];
      WorkingDir = "${drv}/";
      ExposedPorts = { "8080/tcp" = { }; };
      Env = [
        "TWITCH_API_CLIENT_ID=${builtins.getEnv "TWITCH_API_CLIENT_ID"}"
        "TWITCH_API_CLIENT_SECRET=${builtins.getEnv "TWITCH_API_CLIENT_SECRET"}"
      ];
    };
  };

in { inherit drv container; }
