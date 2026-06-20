{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs @ {
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];

      perSystem = {self', ...}: {
        haskellProjects.default = {
          packages = {
          };
          settings = {
          };

          devShell = {
            tools = hp: {
              inherit (hp) alex;
              inherit (hp) happy;
            };
            hlsCheck.enable = true; # Requires sandbox to be disabled
          };
        };

        packages.default = self'.packages.example;
      };
    };
}
