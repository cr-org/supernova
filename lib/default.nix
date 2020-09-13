{ packages ? import ./pkgs.nix { inherit compiler; }, compiler ? "ghc884" }:

let
  inherit (packages) pkgs hp;
  supernova = hp.callCabal2nix "supernova" ./. {};
in
  pkgs.haskell.lib.dontCheck supernova
