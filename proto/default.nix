{ packages ? import ./pkgs.nix { inherit compiler; }, compiler ? "ghc884" }:

let
  inherit (packages) pkgs hp;

  usingProtobuf = pkg: pkgs.haskell.lib.overrideCabal pkg (
    old: {
      buildTools = old.buildTools or [] ++ [ pkgs.protobuf ];
    }
  );

  proto = hp.callCabal2nix "proto" ./. {};
in
  usingProtobuf (
    pkgs.haskell.lib.dontCheck proto
  )
