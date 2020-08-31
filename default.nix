{ pkgs ? import <nixpkgs> {} }:

pkgs.callPackage lib/default.nix {}
