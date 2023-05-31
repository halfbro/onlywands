{
  description = "The place where you show viewers your Noita wands";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    backend.url = "path:./app";
    frontend.url = "path:./ui";
    mod.url = "path:./mod";
  };

  outputs = { self, nixpkgs, flake-utils, backend, frontend, mod }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        backendDrv = backend.packages.${system}.backend;
        frontendDrv = frontend.packages.${system}.frontend;
      in {
        packages.mod = mod.packages.${system}.mod;

        packages.webapp = pkgs.stdenv.mkDerivation {
          name = "onlywands-0.1.1";
          src = ./.;
          installPhase = ''
            mkdir -p $out/tokens
            mkdir -p $out/streamers
            cp ${backendDrv}/bin/backend-app $out/backend-app
            cp -r ${frontendDrv}/* $out/
          '';
        };

        packages.webapp-container = pkgs.dockerTools.buildImage {
          name = "onlywands-dockerized";
          tag = "latest";

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [ self.packages.${system}.webapp pkgs.cacert ];
          };

          config = {
            Cmd = [ "/backend-app" ];
            WorkingDir = "/";
            ExposedPorts = { "8080/tcp" = { }; };
            Env = [
              "TWITCH_API_CLIENT_ID=${builtins.getEnv "TWITCH_API_CLIENT_ID"}"
              "TWITCH_API_CLIENT_SECRET=${
                builtins.getEnv "TWITCH_API_CLIENT_SECRET"
              }"
            ];
            Volumes = {
              "/streamers" = { };
              "/tokens" = { };
            };
          };
        };

        packages.default = self.packages.${system}.webapp;
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          packages = [ pkgs.terraform ];
          shellHook = ''
            source .env
          '';
        };
        devShell = self.devShells.${system}.default;

      });
}
