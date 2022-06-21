{ mkDerivation, base, bytestring, case-insensitive, conduit
, conduit-extra, containers, dhall, directory, filepath
, http-conduit, lens, lib, network-uri, process, random, resourcet
, rio, text, twitter-conduit, twitter-types-lens
}:
mkDerivation {
  pname = "twitterbot";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring case-insensitive conduit conduit-extra containers
    dhall directory filepath http-conduit lens network-uri process
    random resourcet rio text twitter-conduit twitter-types-lens
  ];
  license = lib.licenses.agpl3Plus;
}
