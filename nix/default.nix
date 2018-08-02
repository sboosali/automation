##################################################
{ nixpkgs
, development ? false
}:
##################################################
let

inherit (nixpkgs)
 pkgs
 lib
 stdenv
 buildEnv
 ;

in
##################################################
let

name = "-automation-ENVIRONMENT-";

packages =
 import ./packages {
  inherit pkgs lib;
  inherit development;
 };

environment = buildEnv {
 inherit name; 
 paths = packages; 
};

in
##################################################

stdenv.mkDerivation {
  inherit name;
  env = environment;
  buildInputs = packages;
}

##################################################
## NOTES #########################################
#
# This `nix` file provisions *only* system-dependencies, *not* haskell-dependencies.
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
##################################################