let
  pkgs = import (
    builtins.fetchTarball {
      name   = "nixos-unstable-2020-08-12";
      url    = "https://github.com/NixOS/nixpkgs-channels/archive/f9eba87bf033.tar.gz";
      sha256 = "15cfqd3hargafkhfq5p9hw75dibnbajh06ha69k18scg9l1p1hnl";
    }
  ) {};
in
  pkgs
