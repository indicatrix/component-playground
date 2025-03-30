{
  description = "Component Playground (Elm Library)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" ];
      eachSystem = (f: builtins.listToAttrs
          (builtins.map (system:
            let pkgs = nixpkgs.legacyPackages.${system};
            in { name = system; value = f system pkgs; })
           systems));
  in
  {
    devShells = eachSystem (system: pkgs: {
      default = pkgs.mkShell {
        packages = [
            pkgs.importNpmLock.hooks.linkNodeModulesHook
            pkgs.nodejs
        ];

        npmDeps = pkgs.importNpmLock.buildNodeModules {
            npmRoot = ./.;
            nodejs = pkgs.nodejs;
        };
      };
      # For package updates etc.
      nodejs = pkgs.mkShell {
        buildInputs = [ pkgs.nodejs ];
      };
    });
  };
}
