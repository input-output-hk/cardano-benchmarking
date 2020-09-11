#!/usr/bin/env bash
# shellcheck disable=SC2039,SC2154,SC2206,SC2155,SC2086,SC2034
## Citing the bash BUGS manpage section:
## > Array variables may not (yet) be exported
run_nix_executable() {
        local pkg="$1" exe="$2" extra="$3"
        shift 3

        ## painfully, handle the local args (--build-only and such)
        local extra_clean="${extra/ --build-only/}"
        if test "${extra}" = "${extra_clean}"
        then local build_only=
        else local build_only=t; fi

        local cache_var="$(nix_exe_cache_var "${pkg}" "${exe}")"
        if test -z "${!cache_var}"
        then dprint "executable cache miss for \"${pkg}:${exe}\": ${!nix_executable_cache_@}"
             fill_nix_executable_cache_entry "${pkg}" "${exe}" "${extra_clean}" ||
                     return 1
             test -z "${build_only}" || return 0
        else dprint "executable cache hit for \"${pkg}:${exe}\""
        fi
        dprint "${!cache_var} ${*@Q}"
        ${!cache_var} "$@"
}

nix_exe_cache_var() {
        local pkg="$1" exe="$2"
        echo nix_executable_cache_${pkg//-/_}_${exe//-/_}
}

nix_cache_passthrough() {
        local cache_var
        for cache_var in ${!nix_executable_cache_*}
        do echo "export ${cache_var}=${!cache_var};"
        done
}

cabal_package_to_haskellnix_package_set_name() {
        local cabal_package="$1"

        local nix_pkgset="${CABALPKG_TO_HASKELLNIX_PKGSET[${cabal_package}]}"
        if test -n "${nix_pkgset}"
        then echo -n "${nix_pkgset}"
        else fprint "Cabal exe '${cabal_exe}' unknown:  update CABALEXE_TO_CABALPKG in scripts/libconfig.sh"
             fprint "Cabal package '${cabal_package}' unknown.\nTo fix, add an entry to CABALPKG_TO_HASKELLNIX_PKGSET in scripts/libconfig.sh"
             return 1
        fi
}

fill_nix_executable_cache_entry() {
        local pkg="$1" exe="$2" extra="$3"

        local pkgSet="$(cabal_package_to_haskellnix_package_set_name "${pkg}")"
        test -n "${pkgSet}" || return 1

        local nixattr=${pkgSet}'.'${pkg}'.components.exes.'${exe}

        vprint "filling the Nix executable cache for \"$pkg:$exe\" from ${pkgSet}.."
        NIX_BUILD=(
                nix-build
                "${__COMMON_SRCROOT}/nix"
                --no-out-link
                ${extra}
                ${defaultnix_args}
                -A "${nixattr}"
        )
        dprint "${NIX_BUILD[*]}"
        local out=$("${NIX_BUILD[@]}")
        if test -z "${out}"
        then fprint "failed to build ${nixattr}, rerun with --verbose,"
             fprint "or:  consider if scripts/libconfig.sh : CABALEXE_TO_CABALPKG has a correct entry for ${exe}"
             local __QUOTED="${NIX_BUILD[*]@Q}"
             local __FILTERED=${__QUOTED/" '--no-build-output'"}
             fprint "or:  ${__FILTERED/\" '--quiet'\"}"
             return 1
        fi
        local cache_var="$(nix_exe_cache_var "${pkg}" "${exe}")"
        eval export ${cache_var}
        declare -n nix_cache_write_ref=${cache_var}
        nix_cache_write_ref=${out}/bin/${exe}
        vprint "cached ${exe} = ${!cache_var}"
        dprint "new cache:  ${!nix_executable_cache_@}"
}
