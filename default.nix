{ packages ? import lib/pkgs.nix { inherit compiler; }, compiler ? "ghc884" }:

let
  inherit (packages) pkgs;
in
  pkgs.callPackage lib/default.nix {}
