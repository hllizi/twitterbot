{ mkDerivation, base, bytestring, case-insensitive, conduit
, conduit-extra, containers, dhall, directory, filepath
, http-conduit, lens, lib, network-uri, optparse-applicative
, process, random, resourcet, rio, text, twitter-conduit
, twitter-types-lens, zlib
}:
mkDerivation {
  pname = "twitterbot";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring case-insensitive conduit conduit-extra containers
    dhall directory filepath http-conduit lens network-uri
    optparse-applicative process random resourcet rio text
    twitter-conduit twitter-types-lens
  ];
  executableSystemDepends = [ zlib ];
  license = lib.licenses.agpl3Plus;
}
