let pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = [
    pkgs.abcm2ps
    pkgs.ghc
    pkgs.graphviz
    pkgs.stack
    pkgs.zlib
  ];
  shellHook = ''
  '';
}
