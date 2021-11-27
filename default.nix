{ pkgs, compiler ? "default", ... }:
let

  myHaskellPkgs =
    if compiler == "default" then
      pkgs.haskellPackages
    else
      pkgs.haskell.packages.${compiler};

in
myHaskellPkgs.callCabal2nixWithOptions "AoC" (builtins.fetchGit ./.) "--benchmark" { }
