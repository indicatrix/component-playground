{
  description = "Component Playground (Elm Library)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "aarch64-linux" "x86_64-linux" ];
      eachSystem = (f: builtins.listToAttrs
          (builtins.map (system:
            let pkgs = nixpkgs.legacyPackages.${system};
            in { name = system; value = f system pkgs; })
           systems));
  in
  {
    devShells = eachSystem (system: pkgs:
      let
        inherit (pkgs.elmPackages) elm elm-format elm-json;

        npmDeps = pkgs.importNpmLock.buildNodeModules {
          npmRoot = ./.;
          nodejs = pkgs.nodejs;
          derivationArgs = {
            NODE_EXTRA_CA_CERTS = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
            npmFlags = [ "--ignore-scripts" ];
            postInstall = ''
              # Patch elm-style modules which download binaries in their install
              # scripts by linking directly to nixpkgs versions.
              rm $out/node_modules/elm/bin/elm
              ln -s ${elm}/bin/elm $out/node_modules/elm/bin/elm

              rm $out/node_modules/elm-format/bin/elm-format
              ln -s ${elm-format}/bin/elm-format $out/node_modules/elm-format/bin/elm-format

              rm $out/node_modules/elm-json/bin/elm-json
              ln -s ${elm-json}/bin/elm-json $out/node_modules/elm-json/bin/elm-json
            '';
          };
        };

        dev-elm-typecheck = pkgs.writeShellScriptBin "dev-elm-typecheck" ''
          PLAYGROUND_FLAKE_ROOT="''${PLAYGROUND_FLAKE_ROOT:-$(git rev-parse --show-toplevel)}"
          cd "$PLAYGROUND_FLAKE_ROOT/examples"
          elm make --output /dev/null src/Index.elm
        '';

        dev-check = pkgs.writeShellScriptBin "dev-check" ''
          set -euo pipefail
          hasExt() {
            local extensions="$1"
            local fileChanges="$2"
            grep -E '\.('"''${extensions// /|}"')$' <<< "$fileChanges" || true
          }
          gitChanges=$(git diff --name-only)
          if [ -z "$gitChanges" ]; then
            echo "No changes detected."
            exit 0
          fi
          if [ -n "$(hasExt "elm" "$gitChanges")" ]; then
            echo "==> Elm changes detected, type-checking..."
            dev-elm-typecheck
          fi
        '';

        dev-rebuild-env = pkgs.writeShellScriptBin "dev-rebuild-env" ''
          set -euo pipefail
          PLAYGROUND_FLAKE_ROOT="''${PLAYGROUND_FLAKE_ROOT:-$(git rev-parse --show-toplevel)}"
          cd "$PLAYGROUND_FLAKE_ROOT"
          mkdir -p .nix

          echo "==> Building nix environment..."
          tmpfile=$(mktemp ".nix/.env-cache.sh.XXXXXX")
          trap 'rm -f "$tmpfile"' EXIT

          nix print-dev-env "$PLAYGROUND_FLAKE_ROOT#agent" > "$tmpfile"

          mv "$tmpfile" .nix/env-cache.sh
          trap - EXIT

          echo "==> Activating environment..."
          source .nix/env-cache.sh

          echo "Environment rebuild complete."
        '';

        shellPackages = [
          pkgs.importNpmLock.hooks.linkNodeModulesHook
          pkgs.nodejs
          elm
          elm-format
          elm-json
          dev-elm-typecheck
          dev-check
          dev-rebuild-env
        ];
      in {
        # Interactive dev shell
        default = pkgs.mkShell {
          packages = shellPackages;
          inherit npmDeps;
          shellHook = ''
            export PLAYGROUND_FLAKE_ROOT="$(git rev-parse --show-toplevel)"
          '';
        };

        # Minimal shell for nix print-dev-env — used by dev-rebuild-env
        # to build Claude Code's cached environment (.nix/env-cache.sh).
        # Same deps as default, but no shellHook side effects.
        agent = pkgs.mkShell {
          packages = shellPackages;
          inherit npmDeps;
        };

        # For package updates etc.
        nodejs = pkgs.mkShell {
          buildInputs = [ pkgs.nodejs ];
        };
      });
  };
}
