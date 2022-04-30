{
  description =
    "eclair-haskell: Haskell bindings for Eclair Datalog";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=haskell-updates";
    fu.url = "github:numtide/flake-utils?ref=master";
    ds.url = "github:numtide/devshell?ref=master";
    eclair.url = "github:luc-tielen/eclair-lang";
  };
  outputs = { self, np, fu, ds, eclair }:
    with np.lib;
    with fu.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        ghcVersion = "8107";
        version = "${ghcVersion}.${substring 0 8 self.lastModifiedDate}.${
            self.shortRev or "dirty"
          }";
        config = { };
        overlay = final: _:
          let
            haskellPackages =
              final.haskell.packages."ghc${ghcVersion}".override {
                overrides = hf: hp:
                  with final.haskell.lib; {

                    eclair-lang = with hf;
                      (callCabal2nix "eclair-lang" eclair { }).overrideAttrs
                      (o: { version = "${o.version}.${version}"; });
                  };
              };
          in { inherit haskellPackages; };

        pkgs = import np {
          inherit system config;
          overlays = [ overlay ds.overlay ];
        };
      in with pkgs.lib; rec {
        inherit overlay;
        packages = { inherit (pkgs.haskellPackages) eclair-haskell; };
        defaultPackage = packages.eclair-haskell;
        devShell = pkgs.devshell.mkShell {
          name = "eclair-haskell";
          imports = [];
          packages = with pkgs;
            with haskellPackages; [
              pkgs.ghcid
              (ghcWithPackages (p:
                with p; [
                  hspec-discover
                  ghc
                  cabal-install
                  hsc2hs
                  hpack
                  haskell-language-server
                ]))
            ];
        };
      });
}
