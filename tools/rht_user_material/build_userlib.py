# SPDX-License-Identifier: AGPL-3.0-or-later
"""Build the double-precision RHT USER01 library against an existing GNU SDK.

The SDK source and its archive are not modified. --sdk-build is the CMake
build directory containing the SDK .mod files and libraduser_*.a.
"""
import argparse
import os
from pathlib import Path
import shutil
import subprocess


def main():
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--sdk-build',type=Path,required=True)
    ap.add_argument('--build-dir',type=Path,required=True)
    ap.add_argument('--compiler',default='gfortran')
    ap.add_argument('--archiver',default='ar')
    ap.add_argument('--verify-adapter',action='store_true')
    args=ap.parse_args()
    src=Path(__file__).resolve().parent
    sdk=args.sdk_build.resolve()
    build=args.build_dir.resolve()
    if build==sdk or build==src:
        ap.error('Use a separate output directory')
    build.mkdir(parents=True,exist_ok=True)
    archives=list(sdk.glob('libraduser_*gfortran.a'))
    if len(archives)!=1 or '_sp_' in archives[0].name:
        ap.error('Expected one double-precision GNU SDK archive')
    archive=build/'rht_sdk.a'
    shutil.copyfile(archives[0],archive)
    members=subprocess.check_output([args.archiver,'t',str(archive)],text=True).splitlines()
    replace=[m for m in members if Path(m).stem in ('luser01','lecmuser01')]
    if len(replace)!=2:
        ap.error('Cannot identify the USER01 SDK placeholder objects')
    subprocess.run([args.archiver,'d',str(archive),*replace],check=True)
    library=build/('libraduser_win64.dll' if os.name=='nt' else 'libraduser_linux64.so')
    flags=['-shared','-fPIC','-O2','-fopenmp','-I'+str(sdk)]
    if os.name=='nt':
        flags+=['-fno-underscoring','-static-libgfortran','-static-libgcc','-static']
    subprocess.run([args.compiler,*flags,str(src/'rht_material_mod.F90'),
                    str(src/'lecmuser01.F90'),str(src/'luser01.F90'),
                    '-Wl,--whole-archive',str(archive),'-Wl,--no-whole-archive',
                    '-o',str(library)],cwd=build,check=True)
    if args.verify_adapter:
        test=build/('test_adapter.exe' if os.name=='nt' else 'test_adapter')
        subprocess.run([args.compiler,'-g','-O0','-fcheck=all','-I'+str(sdk),
                        str(src/'rht_material_mod.F90'),str(src/'lecmuser01.F90'),
                        str(src/'luser01.F90'),str(src/'tests/test_adapter.F90'),
                        '-o',str(test)],cwd=build,check=True)
        subprocess.run([str(test)],cwd=build,check=True)
    print(library)


if __name__=='__main__':
    main()
