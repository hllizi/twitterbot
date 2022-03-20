let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          twitterbot = haskellPackages.callPackage ./twitterbot.nix { };
                    };
        };
      };
    };
    pkgs = import <nixpkgs> { inherit config; };
in
  {
    twitterbot = pkgs.haskellPackages.twitterbot;
  }
