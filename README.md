# cardano-benchmarking


## preparation

After first checkout of the repository, update the submodules:
`git submodule update --init`


## building

`stack --nix build --copy-bins`
will compile the project and copy the binaries to the directory ./bin/

