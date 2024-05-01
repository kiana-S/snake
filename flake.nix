{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.haskell-flake.flakeModule ];

      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = { config, system, self', pkgs, ... }: {

        _module.args.pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };

        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc94;
        };

        packages.default = self'.packages.snake;
        apps.default = self'.apps.snake;
      };
    };
}
