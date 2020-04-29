{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  tools   = with haskellPackages; [
    cabal-install
  ];
  
  project = import ./release.nix;
in
  pkgs.mkShell {
    buildInputs = project.env.nativeBuildInputs ++ tools;
  }
