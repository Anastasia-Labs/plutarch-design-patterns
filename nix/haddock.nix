{ pkgs }:

let
  # Use the Haddock sources shipped with the project's compiler, so its
  # interface files remain compatible with the existing dependency docs.
  compiler = pkgs.haskell-nix.compiler.ghc966;
  project = pkgs.haskell-nix.cabalProject' {
    name = "haddock-ghc966";
    compiler-nix-name = "ghc966";
    index-state = "2026-05-31T00:00:00Z";
    src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
      src = compiler.raw-src pkgs;
      subDir = "utils/haddock";
    };
    cabalProject = ''
      packages: . haddock-api haddock-library
      -- The bundled haddock-api Cabal file still has GHC 9.2's base bound.
      allow-newer: haddock-api:base
    '';
    configureArgs = "--disable-tests --disable-benchmarks";
    modules = [
      {
        reinstallableLibGhc = false;
        packages.haddock-api.patches = [ ./patches/haddock-type-data.patch ];
        # Match GHC's bundled Haddock: Template Haskell needs shared RTS symbols.
        packages.haddock.components.exes.haddock.enableExecutableDynamic = true;
      }
    ];
  };
  # Match Haskell.nix's compiler wrappers so interactive Haddock uses the shell's
  # package database, while standalone builds use the pinned compiler by default.
in
pkgs.writeShellScriptBin "haddock" ''
  exec ${project.hsPkgs.haddock.components.exes.haddock}/bin/haddock \
    "-B''${NIX_GHC_LIBDIR:-${compiler}/${compiler.libDir}}" "$@"
''
