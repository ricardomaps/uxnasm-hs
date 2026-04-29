{pkgs ? import <nixpkgs> {}, ...} :

pkgs.mkShell {
  packages = [
    pkgs.ghc
    pkgs.haskell-language-server
  ];
}
