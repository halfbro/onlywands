{
  description =
    "The mod files for the Noita game which will connect and stream wands to onlywands.com";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages.mod = pkgs.stdenv.mkDerivation {
          name = "onlywands-mod-0.1.0";
          src = ./.;

          buildInputs = [ pkgs.zip ];

          installPhase = ''
            mkdir -p $out/onlywands
            cp -r init.lua token.lua settings.lua mod.xml bin files $out/onlywands
            cd $out
            zip -r onlywands.zip onlywands
            cd ..
            rm -r $out/onlywands
          '';
        };

        packages.default = self.packages.${system}.mod;
        defaultPackage = self.packages.${system}.default;

        devShells.default =
          pkgs.mkShell { packages = [ pkgs.sumneko-lua-language-server ]; };
        devShell = self.devShells.${system}.default;
      }

    );

}
