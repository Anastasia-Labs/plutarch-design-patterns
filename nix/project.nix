{ inputs, pkgs, lib }:

pkgs.haskell-nix.cabalProject' ({ ... }: {
  name = "plutarch-design-pattern";

  compiler-nix-name = lib.mkDefault "ghc966";

  src = lib.cleanSource ../.;

  flake.variants = {
    ghc966 = { };
  };

  inputMap = {
    "https://chap.intersectmbo.org/" = inputs.CHaP;
  };

  cabalProjectLocal = ''
    package plutarch-design-pattern
      ghc-options: -Werror
  '';
})
