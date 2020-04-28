fail() {
        echo "ERROR:  $*" >&2
        exit 1
}

nix_binary_for() {
        repo="$1"; shift
        package="$1"; shift
        exe="$1"; shift

        store_path="$(nix-build ../../ext/"${repo}".git/default.nix --no-build-output --no-out-link -A haskellPackages."${package}".components.exes."${exe}")"
        echo "${store_path}"/bin/"${exe}"
}
