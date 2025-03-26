{
  description = "shell env";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, self, nixpkgs, ... }: 
  let
    system = flake-utils.lib.system.x86_64-linux;
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells.${system}.default = pkgs.mkShell {
        name = "env";
        buildInputs = [
          pkgs.jdk21
          pkgs.sbt
        ];
        runScript = "zsh";
    };
  };
}
