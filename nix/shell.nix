{ inputs, pkgs, lib, project, ghc, system, haddock }:

let
  allTools = {
    "ghc966".cabal = project.projectVariants.ghc966.tool "cabal" "latest";
    "ghc966".cabal-fmt = project.projectVariants.ghc966.tool "cabal-fmt" "latest";
    "ghc966".haskell-language-server = project.projectVariants.ghc966.tool "haskell-language-server" "latest";
    "ghc966".stylish-haskell = project.projectVariants.ghc966.tool "stylish-haskell" "latest";
    "ghc966".fourmolu = project.projectVariants.ghc966.tool "fourmolu" "latest";
    "ghc966".hlint = project.projectVariants.ghc966.tool "hlint" "latest";
  };

  tools = allTools.${ghc};

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {
    src = lib.cleanSource ../.;

    hooks = {
      nixpkgs-fmt = {
        enable = false;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = true;
        package = tools.cabal-fmt;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
      };
      fourmolu = {
        enable = true;
        package = tools.fourmolu;
        args = [ ];
      };
      hlint = {
        enable = false;
        package = tools.hlint;
      };
      shellcheck = {
        enable = false;
        package = pkgs.shellcheck;
      };
    };
  };

  commonPkgs = [
    haddock
    tools.haskell-language-server
    tools.stylish-haskell
    tools.fourmolu
    tools.cabal
    tools.hlint
    tools.cabal-fmt

    pkgs.shellcheck
    pkgs.nixpkgs-fmt
    pkgs.github-cli
    pkgs.act
    pkgs.bzip2
    pkgs.gawk
    pkgs.zlib
    # Cabal can rebuild ouroboros-consensus and its cardano-lmdb dependency.
    pkgs.lmdb
    pkgs.cacert
    pkgs.curl
    pkgs.bash
    pkgs.git
    pkgs.gnumake
    pkgs.which
  ] ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.liburing ];
in
{
  inherit preCommitCheck;

  shell = project.shellFor {
    name = "plutarch-design-pattern-${project.args.compiler-nix-name}";

    buildInputs = commonPkgs;
    nativeBuildInputs = [ pkgs.pkg-config ];

    withHoogle = true;

    shellHook = ''
      ${preCommitCheck.shellHook}
      export PATH="${haddock}/bin:$PATH"
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
    '';
  };
}
