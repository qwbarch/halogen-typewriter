{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
  };
  outputs = { nixpkgs, flake-utils, easy-purescript-nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        easy-ps = easy-purescript-nix.packages.${system};
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "halogen-typewriter";
            buildInputs = [
              easy-ps.purs-0_15_9
              easy-ps.spago
              easy-ps.purescript-language-server
              easy-ps.purs-tidy
              #pkgs.nodejs-18_x
              #pkgs.esbuild
            ];
            shellHook = ''
              source <(spago --bash-completion-script `which spago`)
              source <(node --completion-bash)
            '';
          };
        };
      }
    );
}
