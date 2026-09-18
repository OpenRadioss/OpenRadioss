#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/../.." && pwd)

install_prefix=${EIG_INSTALL_PREFIX:-"$repo_root/cbuild_eig_install"}
build_root=${EIG_OPENRADIOSS_BUILD_ROOT:-"$repo_root/cbuild_eig_release"}
jobs=${EIG_BUILD_JOBS:-4}
mpicc=${MPICC:-mpicc}

starter_name=${STARTER_EXEC_NAME:-starter_linux64_gf_eig}
engine_name=${ENGINE_EXEC_NAME:-engine_linux64_gf_ompi_eig}

for command in cmake "$mpicc"; do
  if ! command -v "$command" >/dev/null 2>&1; then
    echo "Required command not found: $command" >&2
    exit 1
  fi
done
if [[ ! -f "$install_prefix/include/slepceps.h" ||
      ! -f "$install_prefix/include/petsc.h" ]]; then
  echo "Run build_eig_dependencies.sh first, or set EIG_INSTALL_PREFIX" >&2
  exit 1
fi

mpi_incdirs=$($mpicc --showme:incdirs)
mpi_libdirs=$($mpicc --showme:libdirs)
read -r mpi_incdir _ <<<"$mpi_incdirs"
read -r mpi_libdir _ <<<"$mpi_libdirs"
if [[ -z "$mpi_incdir" || -z "$mpi_libdir" ]]; then
  echo "Unable to query OpenMPI include and library directories" >&2
  exit 1
fi

starter_build=$build_root/starter
engine_build=$build_root/engine

cmake -S "$repo_root/starter" -B "$starter_build" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$install_prefix" \
  -DEXEC_NAME="$starter_name" \
  -Darch=linux64_gf \
  -Dprecision=dp
cmake --build "$starter_build" --parallel "$jobs"
cmake --install "$starter_build"

cmake -S "$repo_root/engine" -B "$engine_build" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$install_prefix" \
  -DEXEC_NAME="$engine_name" \
  -Darch=linux64_gf \
  -Dprecision=dp \
  -DMPI=ompi \
  -Dmpi_incdir="$mpi_incdir" \
  -Dmpi_libdir="$mpi_libdir" \
  -DOPENRADIOSS_EIG_SLEPC=ON \
  -DSLEPC_ROOT="$install_prefix" \
  -DPETSC_ROOT="$install_prefix"
cmake --build "$engine_build" --parallel "$jobs"
cmake --install "$engine_build"

engine=$install_prefix/bin/$engine_name
if [[ ! -x "$engine" ]]; then
  echo "Installed Engine was not found: $engine" >&2
  exit 1
fi
if command -v readelf >/dev/null 2>&1 &&
   readelf -d "$engine" | grep -qi parmetis; then
  echo "Installed Engine unexpectedly depends directly on ParMETIS" >&2
  exit 1
fi
if command -v ldd >/dev/null 2>&1; then
  loader_output=$(ldd "$engine")
  if grep -q 'not found' <<<"$loader_output"; then
    echo "$loader_output" >&2
    echo "Installed Engine has unresolved shared-library dependencies" >&2
    exit 1
  fi
  if grep -qi parmetis <<<"$loader_output"; then
    echo "$loader_output" >&2
    echo "Installed Engine dependency closure contains ParMETIS" >&2
    exit 1
  fi
fi

cat <<EOF
OpenRadioss eigenvalue build installed in:
  $install_prefix

Starter: $install_prefix/bin/$starter_name
Engine:  $engine
EOF
