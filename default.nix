
# Package       : conveyor
# Description   : Conveyor Belts for Industrial-Grade Pipelines
# Build         : GHC 9.4.2 using Cabal2Nix

{ nixpkgs ? import <nixpkgs> {} }:
    with nixpkgs.haskell.packages.ghc942;
    callCabal2nix "conveyor" ./. {}
