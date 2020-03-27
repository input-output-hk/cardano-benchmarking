
with import ./. {};

haskell.lib.buildStackProject {
  name = "stack-env";
  buildInputs = with pkgs; [ stack zlib openssl gmp libffi git systemd haskellPackages.happy ];
  ghc = (import ../shell.nix {inherit pkgs;}).ghc;
}
