{ pkgs ? (import <nixpkgs> { }), compiler ? "default" }:
let

  aoc-script = pkgs.writeShellScriptBin "aoc" ''
    me=$(basename $0)
    usage() {
        echo -e "Usage:\n" \
            "  $me solve [DAY]\n" \
            "  $me test [DAY]\n" \
            "  $me bench [DAY]\n" \
            "  $me hoogle" >&2
    }

    if [ -z "$1" ]; then
        usage
    fi

    case "$1" in

        "solve")
            if [ -z "$2" ]; then
                cabal run AoC
            else
                cabal run AoC -- "$2"
            fi
            ;;

        "test")
            if [ -n "$2" ]; then
                test="-p $2"
            fi
            find . -name "*.hs" | ${pkgs.entr}/bin/entr -sc \
                            "cabal test --test-options=\"$test --color always\"\
                                        --test-show-details streaming\
                                        --verbose=0"
            ;;

        "bench")

            if [ -n "$2" ]; then
                bench="--benchmark-options=$2"
            fi
            find . -name "*.hs" | ${pkgs.entr}/bin/entr -sc "cabal bench $bench"
            ;;

        "hoogle")
            hoogle server --local
            ;;

    esac
  '';

  myHaskellPkgs =
    if compiler == "default" then
      pkgs.haskellPackages
    else
      pkgs.haskell.packages.${compiler};

  drv = pkgs.callPackage ./default.nix {
    inherit pkgs;
    inherit compiler;
  };

in
myHaskellPkgs.shellFor {
  packages = pkgs: [ drv ];
  genericBuilderArgsModifier = args: args // { doCheck = true; doBenchmark = true; };
  withHoogle = true;
  nativeBuildInputs = with myHaskellPkgs; [
    haskell-language-server
    cabal-install
    aoc-script
  ];
}
