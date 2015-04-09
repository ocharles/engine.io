with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskell-ng.packages.ghc7101.override {
        overrides = self: super: {
                  engine-io = self.callPackage ../engine-io {};
                          socket-io = self.callPackage ./. {};
                                };
                                    };
                                    in modifiedHaskellPackages.socket-io.env
