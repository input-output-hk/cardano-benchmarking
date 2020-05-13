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
        tx_count        =  intOpt 1000       "How many Txs to send, total.";
        add_tx_size     =  intOpt 100        "Extra Tx payload, in bytes.";
        inputs_per_tx   =  intOpt 4          "Inputs per Tx.";
        outputs_per_tx  =  intOpt 4          "Outputs per Tx.";
        tx_fee          =  intOpt 10000000   "Tx fee, in Lovelace.";
        tps             =  intOpt 100        "Strength of generated load, in TPS.";
        init_cooldown   =  intOpt 100        "Delay between init and main submissions.";

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

            "--num-of-txs"             tx_count
            "--add-tx-size"            add_tx_size
            "--inputs-per-tx"          inputs_per_tx
            "--outputs-per-tx"         outputs_per_tx
            "--tx-fee"                 tx_fee
            "--tps"                    tps
            "--init-cooldown"          init_cooldown

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
