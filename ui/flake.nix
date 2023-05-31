{
  description = "Static UI files needed for onlywands.com";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};

      in {
        packages.frontend = let
          mkDerivation = { srcs ? ./elm-srcs.nix, src, name, srcdir ? "./src"
            , targets ? [ ], registryDat ? ./registry.dat
            , outputJavaScript ? false }:
            with pkgs;
            stdenv.mkDerivation {
              inherit name src;

              buildInputs = [ elmPackages.elm ]
                ++ lib.optional outputJavaScript nodePackages.uglify-js;

              buildPhase = pkgs.elmPackages.fetchElmDeps {
                elmPackages = import srcs;
                elmVersion = "0.19.1";
                inherit registryDat;
              };

              installPhase = let
                elmfile = module:
                  "${srcdir}/${
                    builtins.replaceStrings [ "." ] [ "/" ] module
                  }.elm";
                extension = if outputJavaScript then "js" else "html";
              in ''
                mkdir -p $out/static
                ${lib.concatStrings (map (module: ''
                  echo "compiling ${elmfile module}"
                  elm make ${
                    elmfile module
                  } --output $out/${module}.${extension}
                  ${lib.optionalString outputJavaScript ''
                    echo "minifying ${elmfile module}"
                    uglifyjs $out/${module}.${extension} --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
                        | uglifyjs --mangle --output $out/${module}.min.${extension}
                  ''}
                  mv $out/${module}.min.${extension} $out/static
                  rm $out/${module}.${extension}
                '') targets)}
                cp -r resources/* $out/static
              '';
            };
        in mkDerivation {
          name = "frontend-0.1.0";
          src = ./.;
          targets = [ "StreamerPage" ];
          outputJavaScript = true;
        };

        packages.default = self.packages.${system}.frontend;
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.elm2nix
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.zlib
          ];
        };
        devShell = self.devShells.${system}.default;
      });
}
