(import ../. {}).svcLib.defServiceModule
  (lib: with lib;
    let
      nodeConfigDefault = cfg:
        (__fromJSON (__readFile ../../configuration/defaults/generator/configuration.json))
        // {
          "GenesisFile" = cfg.localNodeConf.nodeConfig.GenesisFile;
        };
    in
    { svcName = "tx-generator";
      svcDesc = "configurable transaction generator";

      svcPackageSelector =
        pkgs: ## Local:
              pkgs.cardanoBenchmarkingHaskellPackages.cardano-tx-generator
              ## Imported by another repo, that adds an overlay:
                or pkgs.cardano-tx-generator;
              ## TODO:  that's actually a bit ugly and could be improved.
      ## This exe has to be available in the selected package.
      exeName = "cardano-tx-generator";

      extraOptionDecls = {
        ## TODO: the defaults should be externalised to a file.
        ##
        txCount         =  intOpt 1000       "How many Txs to send, total.";
        addTxSize       =  intOpt 100        "Extra Tx payload, in bytes.";
        inputsPerTx     =  intOpt 4          "Inputs per Tx.";
        outputsPerTx    =  intOpt 4          "Outputs per Tx.";
        txFee           =  intOpt 10000000   "Tx fee, in Lovelace.";
        tps             =  intOpt 100        "Strength of generated load, in TPS.";

        nodeConfig      = attrOpt {}         "Node-style config, overrides the default.";
        keyGen          =  strOpt null       "Signing key: generator";
        keySrc          =  strOpt null       "Signing key: source";
        keyRec          =  strOpt null       "Signing key: receiver";

        localNodeConf   = attrOpt null       "Config of the local observer node";
        targetNodes     = attrOpt null       "Targets: { name = { ip, port } }";

        ## TODO:  obsolete node args
        delegCert       =  strOpt null       "OBSOLETE: delegation certificate";
      };

      configExeArgsFn =
        cfg: with cfg;
          [
            "--config" (__toFile "config-tx-generator.json"
                         (__toJSON (nodeConfigDefault cfg // nodeConfig)))

            "--socket-path"            localNodeConf.socketPath

            "--num-of-txs"             txCount
            "--add-tx-size"            addTxSize
            "--inputs-per-tx"          inputsPerTx
            "--outputs-per-tx"         outputsPerTx
            "--tx-fee"                 txFee
            "--tps"                    tps
            "--sig-key"                keyGen
            "--sig-key"                keySrc
            "--sig-key"                keyRec

            ## TODO:  obsolete node args
            "--genesis-file"           localNodeConf.nodeConfig.GenesisFile
            "--signing-key"            keyGen
            "--delegation-certificate" delegCert
          ] ++
          __attrValues
            (__mapAttrs (name: { ip, port }: "--target-node '(\"${ip}\",${toString port})'")
              targetNodes);

      configSystemdExtraConfig = _: {};

      configSystemdExtraServiceConfig =
        cfg: with cfg; {
          Type = "exec";
          User = "cardano-node";
          Group = "cardano-node";
          Restart = "no";
          RuntimeDirectory = localNodeConf.runtimeDir;
          WorkingDirectory = localNodeConf.stateDir;
        };
    })
