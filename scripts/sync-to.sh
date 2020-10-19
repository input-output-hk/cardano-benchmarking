#!/usr/bin/env bash

set -eo pipefail

this_repo=$(git rev-parse --show-toplevel)
this_project=$this_repo/cabal.project

strict_coherence=
stage_changes=

while test -n "$1"
do case "$1" in
           --strict | --strict-coherence )
                                  strict_coherence=t;;
           --stage | --stage-changes )
                                  stage_changes=t;;
           --help )               usage; exit 1;;
           * ) break;; esac; shift; done
set -u

other_repo=${1:-$(realpath "$this_repo"/../cardano-node)}
other_project=$other_repo/cabal.project
other_name=$(basename "$other_repo")

repos=(
        cardano-node
        ouroboros-network
        iohk-monitoring-framework
)

repo_path() {
        echo -n "../$1"
}

repo_project() {
        echo -n "../$1/cabal.project"
}

dir_nix_hash() {
        local dir=$1
        local commit=$2
        pushd "${dir}" >/dev/null || return
        nix-prefetch-git "file://$(realpath "${dir}")" "${commit}" 2>/dev/null \
                | jq '.sha256' | xargs echo
        popd >/dev/null || return
}

cabal_project_current_commit() {
        local project_file=$1
        local repo_name=$2
        grep "^[ ]*location: .*/${repo_name}\$" "${project_file}" -A1 \
                | tail -n-1 | sed 's/^.* tag: //'
}

cabal_project_current_hash() {
        local project_file=$1
        local repo_name=$2
        grep "^[ ]*location: .*/${repo_name}\$" "${project_file}" -A2 \
                | tail -n-1 | sed 's/^.* --sha256: //'
}

fail() {
    echo "$*" >&2
    exit 1
}

test -r "$other_project" ||
        fail "Usage:  $(basename "$0") [SYNC-FROM-REPO=../${other_name}]"

declare -A repo_commit
declare -A repo_hash
for r in ${repos[*]}
do repo_commit[$r]=$(git -C "$(repo_path "$r")" rev-parse HEAD)
   test -n "${repo_commit[$r]}" || \
           fail "Repository ${r} doesn't have a valid git state."

   repo_hash[$r]=$(dir_nix_hash "$(repo_path "$r")" "${repo_commit[$r]}")
   test -n "${repo_hash[$r]}" || \
           fail "Failed to 'nix-prefetch-git' in $r"
   echo "--( $r:  git ${repo_commit[$r]} / sha256 ${repo_hash[$r]}"

   if test "$r" != "$other_name" -a -n "$strict_coherence" && test "$(cabal_project_current_commit $(repo_project "$other_name") "$r")" != "${repo_commit[$r]}"
   then fail "$(repo_project "$other_name") pins $r to $(cabal_project_current_commit $(repo_project "$other_name") "$r"), but ${repo_commit[$r]} is checked out"
   fi
done

repo_sources_pin_commit() {
        local repo=$1 pin=$2
        jq --arg pin "$pin" '.[$pin].rev' "$repo"/nix/sources.json -r
}

repo_sources_pin_hash() {
        local repo=$1 pin=$2
        jq --arg pin "$pin" '.[$pin].sha256' "$repo"/nix/sources.json -r
}

update_sources_pin() {
        local repo=$1 pin=$2 commit=$3 hash=$4 oldcommit oldhash

        oldcommit=$(repo_sources_pin_commit "$repo" "$pin")
        oldhash=$(repo_sources_pin_hash     "$repo" "$pin")
        if test "$oldcommit" != "$commit" -o "$oldhash" != "$hash"
        then sed -i "s/${oldcommit}/${commit}/" "${repo}"/nix/sources.json
             sed -i "s/${oldhash}/${hash}/"     "${repo}"/nix/sources.json
             cat <<EOF
Updated ${repo}/nix/sources.json pin for $pin:
  ${oldcommit} -> ${commit}
  ${oldhash} -> ${hash}
EOF
        fi
}

## Ensure non-updateable part at committed/indexed state:
git checkout-index --force   "$this_project"
sed -ni '1,/--- >8 ---/  p'  "$this_project"

## Copy over the entire 'source-repository-package' section:
sed -n  '1,/--- 8< ---/! p' "$other_project" |
grep -v   "Please do not" >> "$this_project" || true

## Copy over the index-state
other_state_line=$(sed -n '/index-state:/  p' "$other_project")
sed -i '/index-state:/ s/^.*$/'"$other_state_line"/  "$this_project"

## Update the repo itself
other_commit_old=$(cabal_project_current_commit "$this_project" "$other_name")
other_hash_old=$(cabal_project_current_hash     "$this_project" "$other_name")
sed -i "s/${other_commit_old}/${repo_commit[$other_name]}/" "${this_project}"
sed -i "s/${other_hash_old}/${repo_hash[$other_name]}/"     "${this_project}"

## Update sources.json
update_sources_pin "$this_repo" "$other_name" \
                   "${repo_commit[$other_name]}" \
                   "${repo_hash[$other_name]}"

sources_update_list=(
        haskell.nix
        iohk-nix
)
for pin in ${sources_update_list[*]}
do update_sources_pin "$this_repo" "$pin" \
                      $(repo_sources_pin_commit "$other_repo" "$pin") \
                      $(repo_sources_pin_hash   "$other_repo" "$pin")
done

if test -n "$stage_changes"
then git add "$this_project" "$this_repo"/nix/sources.json
fi
