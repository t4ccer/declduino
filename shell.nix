with import <nixpkgs> {};
mkShell {
  nativeBuildInputs = [
    stack
    haskell-language-server
    ghcid
  ];
}
