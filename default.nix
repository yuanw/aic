{ compiler ? "ghc884" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "aic" =
        hself.callCabal2nix
          "aic"
          (gitignore ./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."aic"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."aic");

  docker = pkgs.dockerTools.buildImage {
    name = "aic";
    config.Cmd = [ "${exe}/bin/aic" ];
  };
in
{
  inherit shell;
  inherit exe;
  #inherit docker;
  inherit myHaskellPackages;
  "aic" = myHaskellPackages."aic";
}
