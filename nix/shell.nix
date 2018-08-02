##################################################
let

nixpkgs =
 import ./nixpkgs.nix;

default =
 import ./default.nix {
  inherit nixpkgs;
  development = true;
};

in
##################################################

default

##################################################