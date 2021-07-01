with import <nixpkgs> {};
mkShell {
  nativeBuildInputs = [
    haskell-language-server
    ghcid
  ];
}
