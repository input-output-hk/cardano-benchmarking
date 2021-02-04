############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ pkgs
, lib
, stdenv
, haskell-nix
, buildPackages
# GHC attribute name
, compiler
# Enable profiling
, profiling ? false
# Link with -eventlog
, eventlog ? false
# Enable asserts for given packages
, assertedPackages ? []
# Version info, to be passed when not building from a git work tree
, gitrev ? null
, libsodium ? pkgs.libsodium
}:
let

  src = haskell-nix.haskellLib.cleanGit {
      name = "cardano-benchmarking-src";
      src = ../.;
  };

  # It is important this matches in both calls to cabalProject or `cabal configure`
  # will run twice.
  cabalProjectLocal = ''
    allow-newer: terminfo:base
  '';

  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject {
      inherit src cabalProjectLocal;
      compiler-nix-name = compiler;
    }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject ({
    inherit src cabalProjectLocal;
    compiler-nix-name = compiler;
    modules = [
      # Allow reinstallation of Win32
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
       nonReinstallablePkgs =
        [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          # ghcjs custom packages
          "ghcjs-prim" "ghcjs-th"
          "ghc-boot"
          "ghc" "array" "binary" "bytestring" "containers"
          "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          # "ghci" "haskeline"
          "hpc"
          "mtl" "parsec" "text" "transformers"
          "xhtml"
          # "stm" "terminfo"
        ];
      })
      {
        # Needed for the CLI tests.
        # Coreutils because we need 'paste'.
        packages.cardano-cli.components.tests.cardano-cli-test.build-tools =
          lib.mkForce [buildPackages.jq buildPackages.coreutils buildPackages.shellcheck];
        packages.cardano-cli.components.tests.cardano-cli-golden.build-tools =
          lib.mkForce [buildPackages.jq buildPackages.coreutils buildPackages.shellcheck];
      }
      {
        # make sure that libsodium DLLs are available for windows binaries:
        packages = lib.genAttrs projectPackages (name: {
          postInstall = lib.optionalString stdenv.hostPlatform.isWindows ''
            if [ -d $out/bin ]; then
              ${setLibSodium}
            fi
          '';
        });
      }
      {
        # Stamp executables with the git revision
        packages = lib.genAttrs
          [ "cardano-tx-generator"
            "locli"
          ]
          (name: {
          components.exes.${name}.postInstall = ''
            ${lib.optionalString stdenv.hostPlatform.isWindows setLibSodium}
            ${setGitRev}
          '';
        });
      }
      ({ pkgs, config, ... }: {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;

        # cardano-cli-test depends on cardano-cli
        packages.cardano-cli.preCheck = "export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}";
      })
      # {
      #   packages = lib.genAttrs projectPackages
      #     (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
      # }
      {
        packages = lib.genAttrs assertedPackages
          (name: { flags.asserts = true; });
      }
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
        # systemd can't be statically linked
        packages.cardano-config.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
        packages.cardano-node.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
      })

      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];
      })
    ];
    # TODO add flags to packages (like cs-ledger) so we can turn off tests that will
    # not build for windows on a per package bases (rather than using --disable-tests).
    # configureArgs = lib.optionalString stdenv.hostPlatform.isWindows "--disable-tests";
  });

  # setGitRev is a postInstall script to stamp executables with
  # version info. It uses the "gitrev" argument, if set. Otherwise,
  # the revision is sourced from the local git work tree.
  setGitRev = ''${haskellBuildUtils}/bin/set-git-rev "${gitrev'}" $out/bin/*'';
  # package with libsodium:
  setLibSodium = "ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll";
  gitrev' = if (gitrev == null)
    then buildPackages.commonLib.commitIdFromGitRepoOrZero ../.git
    else gitrev;
  haskellBuildUtils = buildPackages.haskellBuildUtils.package;
in
  pkgSet
