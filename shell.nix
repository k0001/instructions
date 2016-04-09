{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-5_1" }:

let
  inherit (nixpkgs) pkgs;
  hs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      instructions = self.callPackage ./default.nix {};
    };
  };
  drv = hs.instructions;
in
  if pkgs.lib.inNixShell then drv.env else drv
