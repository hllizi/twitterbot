let bootstrap = import <nixpkgs> { config = {allowBroken = true;};};
    nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
    src = bootstrap.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixpkgs";
            inherit (nixpkgs) rev sha256;
          };
    haskellBuildInputs = with haskellPackages; [
     zlib
     cabal-install
     cabal2nix
     haskell-language-server 
    ];
    nonHaskellBuildInputs = [
      zlib
    ];
in
  with nixpkgs;
mkShell {
  buildInputs = haskellBuildInputs ++ nonHaskellBuildInputs;
    shellHook = ''
     NIXPKGS_ALLOW_BROKEN=1
  '';
}
