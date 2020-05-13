let
  ## TODO: these configs are obviously mocks.  This will get real soon,
  ##       and yes, externalised.
  ##
  delegateConfigs =
    [ { signingKey = "/dev/null"; delegationCertificate = "/dev/null";
        nodeConfig = {
          GenesisFile = "/dev/null";
        };
        hostAddr   = "127.0.0.1"; port = 3001; }
      { signingKey = "/dev/null"; delegationCertificate = "/dev/null";
        nodeConfig = {
          GenesisFile = "/dev/null";
        };
        hostAddr   = "127.0.0.1"; port = 3001; }
      { signingKey = "/dev/null"; delegationCertificate = "/dev/null";
        nodeConfig = {
          GenesisFile = "/dev/null";
        };
        hostAddr   = "127.0.0.1"; port = 3001; }
    ];
in
{
  tx-generator = {
    svcModules = [
      ./nixos/tx-generator-service.nix
    ];
    ## TODO:  see the above todo.
    config = {
      nodeConfig = {
      };
      localNodeConf = {
        socketPath = "/dev/null";
      };
      fundsNodeConf = __head delegateConfigs;
      targetNodes = delegateConfigs;
    };
  };
}
