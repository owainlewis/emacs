#!/usr/bin/env bash
set -euo pipefail

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
target="${HOME}/.emacs.d"
backup="${HOME}/.emacs.d.backup.$(date +%Y%m%d%H%M%S)"

if [[ -e "${target}" && ! -L "${target}" ]]; then
  mv "${target}" "${backup}"
  echo "Backed up ${target} to ${backup}"
elif [[ -L "${target}" ]]; then
  rm "${target}"
fi

ln -s "${repo_dir}" "${target}"
echo "Linked ${repo_dir} to ${target}"
