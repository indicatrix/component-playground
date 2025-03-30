
# Component Library (In development)

## Dev

Enter the default dev-shell with `nix develop`.

Start the vite dev server with `nix develop -c npm run dev`.

Install new packages by updating package.json, running `npm i
--package-lock-only` and re-entering the dev shell.

Run npm executables using `npx <name> <arg> ...`. Eg:
- `npx elm-format` to format elm.
- `npx elm-test` to run elm tests.
- `npx elm-review` to run elm-review (linting).
- `npx tsc` to run typescript type check.
- `npx tsx` to run typescript files.
