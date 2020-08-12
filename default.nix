{ pkgs ? import ./pkgs.nix, compiler ? "ghc883" }:

let
  usingProtobuf = pkg: pkgs.haskell.lib.overrideCabal pkg (
    old: {
      buildTools = old.buildTools or [] ++ [ pkgs.protobuf ];
    }
  );
  # TODO: Override this package to use version 3.1.2.0
  # nixpkgs.haskellPackages.network
in
  usingProtobuf (
    pkgs.haskell.packages.${compiler}.callCabal2nix "hpulsar" ./. {}
  )
