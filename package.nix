{ mkDerivation, aeson, base, blaze-html, bytestring, containers
, data-default, dotenv, fast-logger, hasbolt, hasbolt-extras, hspec
, hspec-contrib, http-client, HUnit, lens, lib, mtl, openapi3
, QuickCheck, random, servant, servant-blaze, servant-client
, servant-client-core, servant-openapi3, servant-server
, servant-swagger-ui, servant-swagger-ui-core, text, time
, transformers, uuid, wai, warp
}:
mkDerivation {
  pname = "neo4j-reaction-api";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base blaze-html bytestring containers data-default dotenv
    fast-logger hasbolt hasbolt-extras lens mtl openapi3 random servant
    servant-blaze servant-openapi3 servant-server servant-swagger-ui
    servant-swagger-ui-core text time transformers uuid
  ];
  executableHaskellDepends = [
    base data-default dotenv fast-logger hasbolt hasbolt-extras mtl
    servant servant-server text time uuid wai warp
  ];
  testHaskellDepends = [
    base data-default dotenv fast-logger hasbolt hasbolt-extras hspec
    hspec-contrib http-client HUnit mtl QuickCheck servant
    servant-client servant-client-core servant-server text time uuid
    wai warp
  ];
  license = lib.licenses.agpl3Only;
  mainProgram = "neo4j-reaction-api";
}
