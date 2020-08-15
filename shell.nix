let
  packages = import ./pkgs.nix {};
  inherit (packages) pkgs hp;

  drv = hp.callCabal2nix "hpulsar" ./. {};
in
  {
    my_project = drv;
    shell = hp.shellFor {
      name = "ghc-shell-for-hpulsar";
      packages = p: [drv];
      buildInputs = with hp; [
        brittany
        cabal-install
        hlint
        pkgs.protobuf
      ];
      shellHook = ''
        export NIX_GHC="$(which ghc)"
        export NIX_GHCPKG="$(which ghc-pkg)"
        export NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
        export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
      '';
    };
  }.shell
