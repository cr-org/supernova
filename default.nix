{ packages ? import ./pkgs.nix { inherit compiler; }, compiler ? "ghc883" }:

let
  inherit (packages) pkgs hp;

  usingProtobuf = pkg: pkgs.haskell.lib.overrideCabal pkg (
    old: {
      buildTools = old.buildTools or [] ++ [ pkgs.protobuf ];
    }
  );
in
  usingProtobuf (
    hp.callCabal2nix "hpulsar" ./. {}
  )
