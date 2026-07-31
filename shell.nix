let pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = [
    pkgs.abcm2ps
    pkgs.exiftool
    pkgs.ghc
    pkgs.gnuplot
    pkgs.graphviz
    pkgs.imagemagick
    pkgs.jpegoptim
    pkgs.optipng
    pkgs.ruff
    pkgs.stack
    pkgs.zlib
    (pkgs.python312.withPackages (python-pkgs: [
      python-pkgs.matplotlib
      python-pkgs.mypy
    ]))
  ];
  shellHook = ''
  '';
}
