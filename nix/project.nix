{ inputs, pkgs, lib, haddock }:

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

  modules = [
    {
      # GHC #25739 is fixed in our Haddock build; keep all dependency docs.
      setupHaddockFlags = [ "--with-haddock=${haddock}/bin/haddock" ];
    }
  ];

  cabalProjectLocal = ''
    package plutarch-design-pattern
      ghc-options: -Werror
  '';
})
