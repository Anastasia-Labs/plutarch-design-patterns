{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  mkShellConfig = ghc:
    import ./shell.nix { inherit inputs pkgs lib project ghc system; };

  ghc966Shell = mkShellConfig "ghc966";

  devShells = rec {
    default = ghc966;
    ghc966 = ghc966Shell.shell;
  };

  projectFlake = project.flake { };

  checks =
    projectFlake.checks
    // {
      formatting = ghc966Shell.preCommitCheck;
    };
in
{
  inherit checks devShells;
  inherit (projectFlake) apps packages;

  # Useful for exploring the resolved Haskell.nix project with `nix repl '.#'`.
  inherit project inputs;
}
