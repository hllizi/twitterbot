let 
  compiler = "ghc8107";
  overlays = [(
    final: prev:
      rec {
        haskell = prev.haskell // { 
          packages = prev.haskell.packages // {
            "${compiler}" = prev.haskell.packages."${compiler}".override 
            {
              overrides = haskellPackagesNew: haskellPackagesOld:
              rec  {
                twitterbot = haskellPackagesNew.callPackage ./twitterbot.nix { };
                twitter-types = haskellPackagesNew.callPackage ./twitter-types.nix { };
              };
            };
          };
        };
      }
    )
  ];

  pkgs = import <nixpkgs> { overlays = overlays; };
in
  {
    twitterbot = pkgs.haskell.packages."${compiler}".twitterbot;
  }
