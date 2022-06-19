let overlay = final: prev: rec {
      haskellPackages = prev.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          twitterbot = haskellPackages.callPackage ./twitterbot.nix { };
          twitter-types = haskellPackages.callPackage ./twitter-types.nix { };
                    };
        };
      };
    pkgs = import <nixpkgs> { overlays = [overlay]; };
in
  {
    twitterbot = pkgs.haskellPackages.twitterbot;
  }
