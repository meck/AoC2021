{ pkgs ? (import <nixpkgs> { }), compiler ? "default" }:
let
  myHaskellPkgs =
    if compiler == "default" then
      pkgs.haskellPackages
    else
      pkgs.haskell.packages.${compiler};

  drv = pkgs.callPackage ./default.nix {
    pkgs = pkgs;
    compiler = compiler;
  };

in
myHaskellPkgs.shellFor {
  packages = pkgs: [ drv ];
  genericBuilderArgsModifier = args: args // { doCheck = true; doBenchmark = true; };
  withHoogle = true;
  nativeBuildInputs = with myHaskellPkgs; [
    haskell-language-server
    cabal-install
    pkgs.inotify-tools
  ];
}
