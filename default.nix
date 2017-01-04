{ callPackage, mkDerivation, base, base64-bytestring, blaze-html, bytestring
, containers, directory, exceptions, extensible-exceptions
, filepath, hslogger, html, HUnit, monad-control, mtl, network
, network-uri, old-locale, parsec, process, sendfile, stdenv, syb
, system-filepath, template-haskell, text, threads, time
, time-compat, transformers, transformers-base, transformers-compat
, unix, utf8-string, xhtml, zlib, cabalsdist
}:
mkDerivation {
  pname = "happstack-server";
  version = "7.4.6.1";
#  src = cabalsdist { pathname = ./.; };
  src = ./.;
  buildDepends = [
    base base64-bytestring blaze-html bytestring containers directory
    exceptions extensible-exceptions filepath hslogger html
    monad-control mtl network network-uri old-locale parsec process
    sendfile syb system-filepath template-haskell text threads time
    time-compat transformers transformers-base transformers-compat unix
    utf8-string xhtml zlib
  ];
  testDepends = [ base bytestring containers HUnit parsec zlib ];
  homepage = "http://happstack.com";
  description = "Web related tools and services";
  license = stdenv.lib.licenses.bsd3;
}
