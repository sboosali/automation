##################################################
{ pkgs
, lib
, development
}:
##################################################
let

buildPackages =
 import ./build.nix {
  inherit pkgs;
};

developmentPackages =
 import ./development.nix {
  inherit pkgs;
};

in
##################################################

buildPackages ++ (lib.optionals development developmentPackages)

##################################################