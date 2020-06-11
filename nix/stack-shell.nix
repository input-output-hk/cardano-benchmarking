
with import ./. {};

haskell.lib.buildStackProject {
  name = "stack-env";
  exactDeps = false;
  buildInputs = with pkgs; [ cabal-install stack zlib openssl gmp libffi git systemd haskellPackages.happy ];
  ghc = (import ../shell.nix {inherit pkgs;}).ghc;
}
