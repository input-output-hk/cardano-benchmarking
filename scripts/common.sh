#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2154,SC2155,SC2034,SC2039,SC2240,SC2124,SC2046
## Don't ask, the story is too sad -- function env sharing & tmux involved.
set -o allexport

export __COMMON_SRCROOT=${__COMMON_SRCROOT:-"$(realpath "$(dirname "${BASH_SOURCE[0]}")"/..)"}
. "${__COMMON_SRCROOT}/scripts/lib.sh" "${__COMMON_SRCROOT}"

usage() {
        cat <<EOF
Usage:

  $(basename "$0") COMMON-FLAGS.. APP-FLAGS.. APP-ARGS..

  Common flags:

    Method to run executables:
    --nix               nix-build default.nix -A haskellPackages.blah....
                          This is the default mode, unless -- read on:
    --cabal             cabal v2-run exe:...
                          Default _iff_ ./dist-newstyle exists.
    --stack             stack run ...
                          Default _iff_ ./.stack-work exists.
    --stack-nix         stack run --nix ...
    --no-path-exes      Forbid using binaries from PATH

    --cls               Clear the TTY before anything happens..

    --[no-]stats        Output end-of-run RTS stats into a .stats file.
    --profile MODE      Enable library & executable profiling, with
                         .prof/.hp and profiteur/profiterole's .html output.
                        The Nix case works end-to-end, while cabal&stack
                        need to be manually set up to provide correspondingly
                        capable binaries before this works.
                        MODE is one of:
       time space space-module space-closure space-type space-retainer space-bio
    --mnemonic-suffix SUFFIX
                        Profiling output will get an additional suffix

    --shelley           Non-HFC mode, Shelley era.  Default
    --byron             Non-HFC mode, Byron era
    --cardano           Cardano/HFC mode, Shelley era

    --quiet             Don't print much.  The default
    --verbose           Be verbose about what's going on
    --debug             Be even more verbose
    --trace             Trace every shell statement

    --help              Print this common help
    --app-help          Print application help (if available)

EOF
}

## Part of common init, see below.
setup_recursion_and_verbosity() {
        lib_recursing=${lib_running}
        export lib_running=t

        if test -z "${verbose}" -a -n "${DEFAULT_VERBOSE}"
        then verbose=t
        fi
        if test -z "${debug}" -a -n "${DEFAULT_DEBUG}"
        then verbose=t; debug=t;
        fi
        if test -z "${trace}" -a -n "${DEFAULT_TRACE}"
        then verbose=t; debug=t; trace=t; set -x
        fi
        export DEFAULT_VERBOSE=${verbose}
        export DEFAULT_DEBUG=${debug}
        export DEFAULT_TRACE=${trace}
}

## Part of common init, see below.
setup_executables() {
        ## This:
        ##   1. decides the mode for executable invocation
        ##   2. performs mode-specific initialisation
        local explain=

        if   test "${cmdline_mode}" != ''
        then explain="of --${SCRIPTS_LIB_SH_MODE} in the command line"
        elif test "${SCRIPTS_LIB_SH_MODE}" != 'default'
        then case "${SCRIPTS_LIB_SH_MODE}" in
             nix | cabal | stack | stack-nix )
             explain="SCRIPTS_LIB_SH_MODE was inherited from the environment";;
             * ) fprint "SCRIPTS_LIB_SH_MODE is not among { cabal nix stack stack-nix }, but: ${SCRIPTS_LIB_SH_MODE}"; exit 1;;
             esac
        elif test -n "${SCRIPTS_LIB_SH_RECURSE_MODE}"
        then SCRIPTS_LIB_SH_MODE=${SCRIPTS_LIB_SH_RECURSE_MODE}
             dprint "inheriting ${SCRIPTS_LIB_SH_RECURSE_MODE} mode in recursed call"
        elif test -d "${__COMMON_SRCROOT}/dist-newstyle"
        then explain="${__COMMON_SRCROOT}/dist-newstyle exists"
             SCRIPTS_LIB_SH_MODE='cabal'
        elif test -d "${__COMMON_SRCROOT}/.stack-work"
        then explain="${__COMMON_SRCROOT}/.stack-work exists"
             SCRIPTS_LIB_SH_MODE='stack'
        else explain="that is the default (use --cabal or --stack to override)"
             SCRIPTS_LIB_SH_MODE=$default_mode
        fi
        export SCRIPTS_LIB_SH_RECURSE_MODE=${SCRIPTS_LIB_SH_MODE}
        oprint_top "${SCRIPTS_LIB_SH_MODE} mode, because ${explain}."

        if test -n "${profile}"
        then vprint_top "cardano-node profiling enabled:  ${profile}"
        fi
        export COMMON_NODE_PROFILING=${profile}

        case ${SCRIPTS_LIB_SH_MODE} in
                nix ) setup_nix;;
        esac
}

setup_nix() {
        if test -z "${profile}"
        then defaultnix_config=''
        else defaultnix_config='{haskellNix={profiling=true;};}'
        fi
        dprint "default.nix config: ${defaultnix_config}"

        defaultnix_args="--argstr gitrev $(git rev-parse HEAD)"
        if test -n "${defaultnix_config}"
        then defaultnix_args+=" --arg config ${defaultnix_config}"
        fi
}

###
### Common main
###

##   1. set up global state
##   2. handle common CLI args, and yield upon seeing
##      anything we have no idea about, thereby allowing for
##      downstream, case-specific parsing of remaining args.
##   3. resolve verbosity and recursion issues

export scripts="${__COMMON_SRCROOT}/scripts"

export era=${era:-'shelley'}

export profile=
export verbose=
export debug=
export allow_path_exes=${allow_path_exes:-'yes'}

default_mode='nix'

SCRIPTS_LIB_SH_MODE=${SCRIPTS_LIB_SH_MODE:-default}
cmdline_mode=
mnemonic_suffix=${mnemonic_suffix:-}
force_genesis=

while test -n "$1"
do case "$1" in
           --nix )                SCRIPTS_LIB_SH_MODE='nix'; cmdline_mode=t;;
           --cabal )              SCRIPTS_LIB_SH_MODE='cabal'; cmdline_mode=t;;
           --stack )              SCRIPTS_LIB_SH_MODE='stack'; cmdline_mode=t;;
           --stack-nix )          SCRIPTS_LIB_SH_MODE='stack-nix'; cmdline_mode=t;;
           --no-path-exes )       allow_path_exes=no;;

           --cls )                echo -en "\ec";;

           --profile )            profile=$2; shift;;
           --mnemonic-suffix )    mnemonic_suffix=$2; shift;;

           ## Should be moved to lib-node.sh?
           --force-genesis )      force_genesis=t;;

           --shelley )            era='shelley';;
           --byron )              era='byron';;
           --cardano )            era='cardano-shelley';;

           --quiet )              verbose=;;
           --verbose )            verbose=t;;
           --debug )              debug=t; verbose=t;;
           --trace )              debug=t; verbose=t; trace=t; set -x;;

           --help )               usage; exit 1;;
           * ) break;; esac; shift; done
export SCRIPTS_LIB_SH_MODE

export configuration="${__COMMON_SRCROOT}/configuration-$era"

setup_recursion_and_verbosity
setup_executables

vprint_top "git commit:  $(git rev-parse HEAD)"
vprint_top "process group id (PGID): $$"
vprint_top "to list the process tree:  pstree -Tulap $$"
vprint_top "..or, interactively:       watch --interval=1 \"pstree -Tulap $$\""
