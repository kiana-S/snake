{
  description = "A snake game in Haskell using Yampa";

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [];
        pkgs = nixpkgs.legacyPackages.${system};
        
        # package/executable name
        packageName = "snake";
        execName = packageName;
        
        # version of ghc used
        hp = pkgs.haskellPackages;
        
        project = returnShellEnv:
          hp.developPackage {
            inherit returnShellEnv;
            name = packageName;
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              # Use callCabal2nix to override Haskell dependencies here
              # cf. https://tek.brick.do/K3VXJd8mEKO7
              # Example: 
              # > NanoID = self.callCabal2nix "NanoID" inputs.NanoID { };
              # Assumes that you have the 'NanoID' flake input defined.
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with hp; [
                # Specify your build/dev dependencies here.
              ]);
          };
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        defaultApp = {
          type = "app";
          program = "${self.defaultPackage.${system}}/bin/${execName}";
        };

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
