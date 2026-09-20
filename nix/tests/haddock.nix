{ pkgs, haddock }:

# The upstream GHC #25739 reproducer exports a type-data constructor on its own.
pkgs.runCommand "haddock-type-data-regression" { nativeBuildInputs = [ haddock ]; } ''
  mkdir -p "$out"
  cat > Bug25739.hs <<'EOF'
  {-# LANGUAGE TypeData #-}
  module Bug25739 (Bar) where
  type data Foo = Bar
  EOF

  haddock --html --hoogle --hyperlinked-source --quickjump \
    --package-name=haddock-type-data-regression \
    --dump-interface="$out/Bug25739.haddock" --odir="$out" Bug25739.hs

  test -s "$out/Bug25739.haddock"
  test -s "$out/src/Bug25739.html"
  grep -q 'id="t:Bar"' "$out/Bug25739.html"
  grep -q '^data Bar :: Foo$' "$out/haddock-type-data-regression.txt"
''
