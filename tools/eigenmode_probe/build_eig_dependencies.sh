#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/../.." && pwd)

build_root=${EIG_DEPS_BUILD_ROOT:-"$repo_root/cbuild_eig_dependencies"}
install_prefix=${EIG_INSTALL_PREFIX:-"$repo_root/cbuild_eig_install"}
jobs=${EIG_BUILD_JOBS:-4}
run_checks=${EIG_RUN_DEPENDENCY_CHECKS:-1}

mpicc=${MPICC:-mpicc}
mpicxx=${MPICXX:-mpic++}
mpifort=${MPIFORT:-mpifort}

petsc_tag=v3.25.5
petsc_commit=a0b507cc1cc9eac739c34d6e71521f4854e84e36
slepc_tag=v3.25.1
slepc_commit=9b3705b7c08f4a75e6e7435dbbcd6d6033609458
petsc_arch=arch-openradioss-eig

claim_directory() {
  local path=$1
  local marker=$2
  local existing=

  if [[ -d "$path" && ! -f "$path/$marker" ]]; then
    existing=$(find "$path" -mindepth 1 -maxdepth 1 -print -quit)
    if [[ -n "$existing" ]]; then
      echo "Refusing to use non-empty unmarked directory: $path" >&2
      exit 1
    fi
  fi
  mkdir -p "$path"
  printf '%s\n' 'Owned by build_eig_dependencies.sh' >"$path/$marker"
}

for command in git make python3 "$mpicc" "$mpicxx" "$mpifort"; do
  if ! command -v "$command" >/dev/null 2>&1; then
    echo "Required command not found: $command" >&2
    exit 1
  fi
done

# OpenMPI 5 launches through a companion prterun executable beside mpicc.
# Keep the whole selected MPI toolchain on PATH when callers pass absolute
# wrapper paths, otherwise PETSc's MPI self-tests can compile but not run.
mpi_bin_dir=$(dirname -- "$(command -v "$mpicc")")
export PATH="$mpi_bin_dir:$PATH"

clone_pinned() {
  local url=$1
  local tag=$2
  local commit=$3
  local destination=$4

  if [[ ! -d "$destination/.git" ]]; then
    if [[ -e "$destination" ]]; then
      echo "Refusing to replace non-Git path: $destination" >&2
      exit 1
    fi
    git clone --branch "$tag" --depth 1 "$url" "$destination"
  fi
  if [[ $(git -C "$destination" rev-parse HEAD) != "$commit" ]]; then
    echo "$destination is not the approved $tag commit $commit" >&2
    exit 1
  fi
  if ! git -C "$destination" diff --quiet ||
     ! git -C "$destination" diff --cached --quiet; then
    echo "Refusing to build from a modified dependency tree: $destination" >&2
    exit 1
  fi
}

claim_directory "$build_root" .openradioss-eig-dependency-build
claim_directory "$install_prefix" .openradioss-eig-install-prefix
petsc_source=$build_root/petsc
slepc_source=$build_root/slepc

clone_pinned https://gitlab.com/petsc/petsc.git \
  "$petsc_tag" "$petsc_commit" "$petsc_source"
clone_pinned https://gitlab.com/slepc/slepc.git \
  "$slepc_tag" "$slepc_commit" "$slepc_source"

cd "$petsc_source"
python3 ./configure \
  PETSC_ARCH="$petsc_arch" \
  --prefix="$install_prefix" \
  --with-debugging=0 \
  --with-shared-libraries=1 \
  --with-scalar-type=real \
  --with-precision=double \
  --with-64-bit-indices=0 \
  --with-cc="$mpicc" \
  --with-cxx="$mpicxx" \
  --with-fc="$mpifort" \
  COPTFLAGS=-O3 \
  CXXOPTFLAGS=-O3 \
  FOPTFLAGS=-O3 \
  --with-openmp=0 \
  --with-x=0 \
  --with-hwloc=0 \
  --with-parmetis=0 \
  --with-ptscotch=0 \
  --download-openblas=1 \
  --download-metis=1 \
  --download-scalapack=1 \
  --download-mumps=1

make PETSC_DIR="$petsc_source" PETSC_ARCH="$petsc_arch" -j "$jobs" all
if [[ "$run_checks" == 1 ]]; then
  make PETSC_DIR="$petsc_source" PETSC_ARCH="$petsc_arch" check
fi
make PETSC_DIR="$petsc_source" PETSC_ARCH="$petsc_arch" install

cd "$slepc_source"
PETSC_DIR="$install_prefix" python3 ./configure \
  --prefix="$install_prefix" --with-clean
make SLEPC_DIR="$slepc_source" PETSC_DIR="$install_prefix" -j "$jobs" all
if [[ "$run_checks" == 1 ]]; then
  make SLEPC_DIR="$slepc_source" PETSC_DIR="$install_prefix" check
fi
make SLEPC_DIR="$slepc_source" PETSC_DIR="$install_prefix" install

petsc_config=$install_prefix/include/petscconf.h
if ! grep -Eq '^#define[[:space:]]+PETSC_HAVE_MUMPS[[:space:]]+1' \
    "$petsc_config"; then
  echo "Installed PETSc does not report MUMPS support" >&2
  exit 1
fi
if grep -Eq '^#define[[:space:]]+PETSC_HAVE_PARMETIS[[:space:]]+1' \
    "$petsc_config"; then
  echo "ParMETIS was unexpectedly enabled; this build is not redistributable" >&2
  exit 1
fi

petsc_library=$(find "$install_prefix" -maxdepth 3 -type f \
  -name 'libpetsc.so*' -print -quit)
if [[ -z "$petsc_library" ]]; then
  echo "Installed PETSc shared library was not found" >&2
  exit 1
fi
if command -v readelf >/dev/null 2>&1 &&
   readelf -d "$petsc_library" | grep -qi parmetis; then
  echo "Installed PETSc still has a runtime dependency on ParMETIS" >&2
  exit 1
fi

license_dir=$install_prefix/share/licenses/openradioss-eig
external=$petsc_source/$petsc_arch/externalpackages
mkdir -p "$license_dir"
install -m 0644 "$petsc_source/LICENSE" "$license_dir/PETSc-LICENSE"
install -m 0644 "$slepc_source/LICENSE.md" "$license_dir/SLEPc-LICENSE.md"
install -m 0644 "$external/MUMPS_5.8.2/LICENSE" \
  "$license_dir/MUMPS-LICENSE"
install -m 0644 "$external/git.metis/LICENSE.txt" \
  "$license_dir/METIS-LICENSE.txt"
install -m 0644 "$external/git.scalapack/LICENSE" \
  "$license_dir/ScaLAPACK-LICENSE"
openblas_license=$(find "$external" -maxdepth 2 -type f \
  -iname LICENSE -ipath '*openblas*' -print -quit)
if [[ -z "$openblas_license" ]]; then
  echo "OpenBLAS licence file was not found" >&2
  exit 1
fi
install -m 0644 "$openblas_license" "$license_dir/OpenBLAS-LICENSE"

cat <<EOF
Eigenvalue dependencies installed in:
  $install_prefix

PETSc: $petsc_tag ($petsc_commit)
SLEPc: $slepc_tag ($slepc_commit)
MUMPS: 5.8.2 (selected and pinned by PETSc)
ParMETIS: disabled
EOF
