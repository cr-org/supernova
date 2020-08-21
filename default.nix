{ packages ? import ./pkgs.nix { inherit compiler; }, compiler ? "ghc883" }:

let
  inherit (packages) pkgs hp;

  usingProtobuf = pkg: pkgs.haskell.lib.overrideCabal pkg (
    old: {
      buildTools = old.buildTools or [] ++ [ pkgs.protobuf ];
    }
  );

  supernova = hp.callCabal2nix "supernova" ./. {};
in
  usingProtobuf (
    pkgs.haskell.lib.dontCheck supernova
  )
