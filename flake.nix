{
  description = "A snake game in Haskell using Dunai";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # package/executable name
        packageName = "snake";
        execName = packageName;
        
        # version of ghc used
        hp = pkgs.haskell.packages.ghc92;
        
        project = returnShellEnv:
          hp.developPackage {
            inherit returnShellEnv;
            name = packageName;
            root = ./.;
            withHoogle = false;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with hp; [
                # Specify your build/dev dependencies here.
                hlint
                haskell-language-server
                ormolu

                pkgs.mesa
                pkgs.mesa_glu
                pkgs.freeglut
              ]);
          };
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        packages.default = project false;

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/${execName}";
        };

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
