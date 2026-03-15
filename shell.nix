let pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = [
    pkgs.abcm2ps
    pkgs.ghc
    pkgs.graphviz
    pkgs.gnuplot
    pkgs.stack
    pkgs.zlib
    pkgs.ruff
    (pkgs.python312.withPackages (python-pkgs: [
      python-pkgs.matplotlib
      python-pkgs.mypy
    ]))
  ];
  shellHook = ''
  '';
}
