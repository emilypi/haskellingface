{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  haskellPackages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};
  package = haskellPackages.callCabal2nix "huggingface" ./. {};
in
  if nixpkgs.lib.inNixShell then package.env else package
