{ mkDerivation, aeson, amazonka-core, async, base, base64
, blaze-html, blaze-markup, bytestring, Cabal, cheapskate
, directory, dotenv, feed, filepath, frontmatter, fsutils
, github-rest, hspec, hspec-core, hspec-expectations, http-client
, http-client-tls, http-types, mmark, mtl, mysql, pandoc
, QuickCheck, random, req, serverless-haskell, stdenv, stringsearch
, text, time, transformers, transformers-base, unordered-containers
, vector, wai, wai-app-static, wai-extra, warp, webdriver
, xml-conduit
}:
mkDerivation {
  pname = "websites";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  doCheck = false;
  executableHaskellDepends = [
    aeson amazonka-core base base64 blaze-html blaze-markup bytestring
    Cabal cheapskate directory dotenv feed filepath frontmatter fsutils
    github-rest http-types mmark mtl mysql pandoc req
    serverless-haskell stringsearch text time transformers
    unordered-containers wai wai-app-static wai-extra warp xml-conduit
  ];
  testHaskellDepends = [
    aeson async base blaze-html blaze-markup bytestring Cabal
    cheapskate directory dotenv feed filepath frontmatter fsutils hspec
    hspec-core hspec-expectations http-client http-client-tls
    http-types mmark mtl pandoc QuickCheck random req text time
    transformers transformers-base vector wai wai-app-static wai-extra
    warp webdriver xml-conduit
  ];
  homepage = "https://github.com/danwdart/projects#readme";
  license = stdenv.lib.licenses.agpl3;
}
