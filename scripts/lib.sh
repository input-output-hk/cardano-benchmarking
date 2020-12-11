#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2034,SC2154,SC2039,SC1007,SC2207,SC2145,SC2155,SC2206
## Don't import this file directly.

## TODO:  debug the spectacular failure this causes..
# set -e

. "${__COMMON_SRCROOT}/scripts/libconfig.sh"
. "${__COMMON_SRCROOT}/scripts/lib-cli.sh"
. "${__COMMON_SRCROOT}/scripts/lib-nix.sh"
. "${__COMMON_SRCROOT}/scripts/lib-node.sh"

##
## This depends on the setup done by scripts/common.sh
##

oprint() {
        echo "--( $*" >&2
}
export -f oprint
oprint_top() {
        ## This only prints if ran from the top-level shell process.
        if test -z "${lib_recursing}"; then oprint "$@"; fi
}
export -f oprint_top

vprint() {
        if test -n "${verbose}${debug}"; then echo "-- $*" >&2; fi
}
export -f vprint
vprint_top() {
        ## This only prints if either in debug mode,
        ## or ran from the top-level shell process.
        if test -z "${lib_recursing}" -o -n "${debug}"; then vprint "$@"; fi
}
export -f vprint_top

dprint() {
        if test -n "${debug}"; then echo "-- $*" >&2; fi
}
export -f dprint

fprint() {
        echo "-- FATAL:  $*" >&2
}
export -f fprint

fail() {
        fprint "$@"
        exit 1
}

prebuild() {
        local exe="$1"
        oprint "prebuilding the \"${exe}\" executable in \"${SCRIPTS_LIB_SH_MODE}\" mode.."
        run --build-only "${exe}"
}
export -f prebuild

run() {
        if test -n "${verbose}"
        then run_verbose "$@"
        else run_quiet   "$@"
        fi
}
export -f run

run_verbose() {
        actually_run "$@"
}
export -f run_verbose

run_quiet()
{
        ## This nightmare below (minus the last line) does one simple thing:
        ##   ..it ensures the --build-extra argument has the mode-specific argument
        ##   that muffles output from the mode-appropriate build tool.
        local bld_extra=
        while test -n "$1"
        do case "$1" in
           --build-extra )    bld_extra=$2; shift;;
           * ) break;; esac; shift; done
        case ${SCRIPTS_LIB_SH_MODE} in
                nix )               bld_extra="--no-build-output --quiet ${bld_extra}";;
                cabal )             bld_extra="${bld_extra}";;
        esac

        actually_run --build-extra "${bld_extra}" "$@"
}
export -f run_quiet

cabal_exe_to_cabal_pkg() {
        local cabal_exe="$1"

        local cabal_pkg="${CABALEXE_TO_CABALPKG[${cabal_exe}]}"
        if test -n "${cabal_pkg}"
        then echo -n "${cabal_pkg}"
        else fprint "Cabal exe '${cabal_exe}' unknown.\nTo fix, add an entry to CABALEXE_TO_CABALPKG in scripts/libconfig.sh"
             return 1
        fi
}

###
### TODO:  untangle this mess -- factor and introduce dispatch
###
actually_run()
{
        local ARGS=("$@")
        dprint "actually_run:  ${ARGS[@]@Q}"
        local toolargs=
        local profile= profile_suffix= user_suffix= profile_prefix= profile_file=
        local profile_root='./profile'
        local profmode=
        local rtsopts=
        local stats=
        local tag=
        local exe=
        local pkg=
        local main_args_processed=
        local build_only=
        while test ${#ARGS[@]} -ge 1
        do ## oprint "args:   ${ARGS[@]@Q}"
           case "${ARGS[0]}" in
           --build-extra )       extra="${ARGS[1]}";        ARGS=("${ARGS[@]:1}");;
           --profile )           profile=${ARGS[1]};        ARGS=("${ARGS[@]:1}");;
           --profile-suffix )    user_suffix=.${ARGS[1]};   ARGS=("${ARGS[@]:1}");;
           --stats )             stats=t;;
           --no-stats )          stats=;;
           --build-only )        build_only=t;;
           --tag ) case ${ARGS[1]} in
                           git-head | HEAD ) tag="$(git symbolic-ref HEAD | sed 's,.*/,,g')";;
                           * )               tag=${ARGS[1]};; esac
                   ARGS=("${ARGS[@]:1}");;
           --* ) fprint "actually_run:  unhandled arg '${ARGS[0]}'"; return 1;;
           * ) if test -n "${main_args_processed}"
               then break; else main_args_processed=t; fi

               exe="${ARGS[0]}"; ARGS=(${ARGS[@]:1})
               pkg="$(cabal_exe_to_cabal_pkg "${exe}")"
               test -n "${pkg}" || return 1

               ## Handle per-executable extra options -- see libconfig.sh
               ## This effectively adds a second round of args processing.
               ##
               ## Note, this complication is because we can't possibly know the
               ## exe name before we've processed the arglist.
               extra_exe_opts="${CABALEXE_TO_LIBOPTS[${exe}]}"
               if test -n "${extra_exe_opts}"
               then ARGS=(${extra_exe_opts} ${ARGS[@]})
                    vprint "Injected extra options for exe '${exe}':  ${extra_exe_opts}"
                    continue ## Have another go.
               else break; fi;; esac
           ARGS=("${ARGS[@]:1}"); done

        dprint "actually_run application exe args:  ${ARGS[*]}"

        rtsopts="+RTS";
        case "${profile}" in
           time )            vprint "profiling:  time"
                             profmode='-P';  profile_suffix="prof";;
           space )           vprint "profiling:  space, by default cost centre"
                             profmode='-h';  profile_suffix="hp";;
           space-module )    vprint "profiling:  space, by module"
                             profmode='-hm'; profile_suffix="hp";;
           space-closure )   vprint "profiling:  space, by closure"
                             profmode='-hd'; profile_suffix="hp";;
           space-type )      vprint "profiling:  space, by type"
                             profmode='-hy'; profile_suffix="hp";;
           space-retainer )  vprint "profiling:  space, by retainer"
                             profmode='-hr'; profile_suffix="hp";;
           space-bio )       vprint "profiling:  space, biographic"
                             profmode='-hb'; profile_suffix="hp";
                             rtsopts+=' -N1';;
           '' )              true;;
           * ) fprint "--profile requires a mode argument:  time space space-module space-closure space-type space-retainer space-bio"; return 1;; esac

        if test -n "${stats}${profile}"
        then mkdir -p "${profile_root}"
             profile_prefix="${profile_root}/$(generate_mnemonic "${tag}")${user_suffix}"
        fi
        if test -n "${stats}"
        then rtsopts+=" --machine-readable -t${profile_prefix}.stats"
        fi
        if test -n "${profile}"
        then profile_file="${profile_prefix}${profile_suffix}"
             # rtsopts="+RTS --machine-readable -l -t${profile_prefix}.stats -po${profile_prefix} -ol ${profile_prefix}.eventlog ";
             # Sadly GHC 8.6 doesn't support -ol
             rtsopts+=" -l -po${profile_prefix} ";
        fi
        rtsopts+="${profmode} -RTS";
        if test "${rtsopts}" = "+RTS -RTS"
        then rtsopts=; fi
        vprint "RTS options:    ${rtsopts}"
        if test -n "${rtsopts}"
        then vprint "result prefix:  ${profile_prefix}"; fi

        ## Handle --build-only (ugh..)
        if test -n "${build_only}"
        then local rob='build' dash2=
        else local rob='run'   dash2="--"
        fi

        local CMD=()
        case ${SCRIPTS_LIB_SH_MODE} in
        nix )       ## Disallow using binaries from PATH for Nix method:
                    allow_path_exes='no'
                    CMD=(run_nix_executable              $pkg     $exe
                         "${toolargs}${build_only:+ --build-only}");;
        cabal )     CMD=(cabal v2-${rob} ${toolargs} -v0 $pkg:exe:$exe ${dash2});;
        * ) echo "INTERNAL ERROR: unknown mode:  $SCRIPTS_LIB_SH_MODE" >&2; return 1;;
        esac
        if test -z "${build_only}"
        then if test "${allow_path_exes}" = 'yes' -a -n "$(command -v "$exe")"
             then vprint "Using $exe from PATH:  $(command -v "$exe")"
                  CMD=($exe)
             fi
             CMD+=(${rtsopts} "${ARGS[@]}")
        elif test "${allow_path_exes}" = 'yes' && command -v "$exe"
        then return 0
        fi

        vprint "${CMD[@]}"
        "${CMD[@]}"
        local status=$?

        if test  -f "${exe}.eventlog"
        then mv "${exe}.eventlog" "${profile_prefix}.eventlog"
        fi
        if test -f "${profile_file}" -a -x "$(command -v profiteur 2>/dev/null)"
        then profiteur "${profile_file}"
        fi
        if test -f "${profile_file}" -a -x "$(command -v profiterole 2>/dev/null)"
        then profiterole "${profile_file}"
        fi
        if test -f "${profile_file}" -a -x "$(command -v eventlog2html 2>/dev/null)"
        then eventlog2html "${profile_prefix}.eventlog"
        fi

        return ${status}
}
export -f actually_run

##
## Misc stuff, too few to be worth splitting out
##
generate_wordpair() {
        case ${SCRIPTS_LIB_SH_MODE} in
                nix ) nix-shell -p diceware --run 'diceware --no-caps --num 2 --wordlist en_eff -d-' 2>/dev/null || true;;
                * ) true;; esac
}

generate_mnemonic()
{
        local mnemonic="${1:-$(generate_wordpair)}"
        local timestamp="$(date +%s)"
        local commit="$(git rev-parse HEAD | cut -c-8)"
        local status=''

        if git diff --quiet --exit-code
        then status=pristine
        else status=modified
        fi

        echo "${timestamp}.${commit}.${status}${mnemonic:+.${mnemonic}}"
}
export -f generate_mnemonic
