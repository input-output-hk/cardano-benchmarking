# Create topology files for NNODES
./topology-generator.sh
wait

# Create configuration files for NNODES
./configuration-generator.sh
wait

# Create Genesis based on parameters
./prepare_genesis-generic.sh
wait

# Run the nodes
./start-generic.sh
