root_dir=${PWD}/..

rm *.o
rm *.mod

icx -O2  -std=c++11  -MD -MT  ./compare_cand.cpp.o -c ../source/interfaces/intsort/compare_cand.cpp
icx -O2  -std=c++11  -MD -MT  ./unlimit_stack.cpp.o -c unlimit_stack.cpp

ifort  -I${root_dir}/../common_source/includes -I${root_dir}/share/includes -I${root_dir}/share/r8 -I${root_dir}/share/spe_inc -I${root_dir}/cbuild_engine_linux64_intel_impi/CMakeFiles/includes_engine_linux64_intel_impi -module ./  -axSSE3,COMMON-AVX512 -no-fma -O3 -fp-model precise -fimf-use-svml=true -qopenmp -DMYREAL8 -ftz  -extend-source -assume buffered_io -align array64byte -DMPI -DNO_SERIALIZE -I/opt/intel/oneapi/mpi/2021.11/include/    -DCPP_mach=CPP_p4linux964 -DCPP_rel=00  -g -traceback -c ${root_dir}/../common_source/modules/constant_mod.F -o constant_mod.F.o 

ifort  -I${root_dir}/../common_source/includes -I${root_dir}/share/includes -I${root_dir}/share/r8 -I${root_dir}/share/spe_inc -I${root_dir}/cbuild_engine_linux64_intel_impi/CMakeFiles/includes_engine_linux64_intel_impi -module ./  -axSSE3,COMMON-AVX512 -no-fma -O3 -fp-model precise -fimf-use-svml=true -qopenmp -DMYREAL8 -ftz  -extend-source -assume buffered_io -align array64byte -DMPI -DNO_SERIALIZE -I/opt/intel/oneapi/mpi/2021.11/include/    -DCPP_mach=CPP_p4linux964 -DCPP_rel=00  -g -traceback -c ${root_dir}/source/interfaces/intsort/collision_mod.F -o ./collision_mod.F.o

ifort  -I${root_dir}/../common_source/includes -I${root_dir}/share/includes -I${root_dir}/share/r8 -I${root_dir}/share/spe_inc  -module ./  -axSSE3,COMMON-AVX512 -no-fma -O3 -fp-model precise -fimf-use-svml=true -qopenmp -DMYREAL8 -ftz  -extend-source -assume buffered_io -align array64byte -DMPI -DNO_SERIALIZE -I/opt/intel/oneapi/mpi/2021.11/include/    -DCPP_mach=CPP_p4linux964 -DCPP_rel=00  -g -traceback -c ${root_dir}/source/interfaces/int07/inter7_gather_cand.F -o ./inter7_gather_cand.F.o

ifort  -I${root_dir}/../common_source/includes -I${root_dir}/share/includes -I${root_dir}/share/r8 -I${root_dir}/share/spe_inc  -module ./  -axSSE3,COMMON-AVX512 -no-fma -O3 -fp-model precise -fimf-use-svml=true -qopenmp -DMYREAL8 -ftz  -extend-source -assume buffered_io -align array64byte -DMPI -DNO_SERIALIZE -I/opt/intel/oneapi/mpi/2021.11/include/    -DCPP_mach=CPP_p4linux964 -DCPP_rel=00  -g -traceback -c ${root_dir}/source/interfaces/intsort/inter7_penetration.F -o ./inter7_penetration.F.o

ifort  -I${root_dir}/../common_source/includes -I${root_dir}/share/includes -I${root_dir}/share/r8 -I${root_dir}/share/spe_inc  -module ./  -axSSE3,COMMON-AVX512 -no-fma -O3 -fp-model precise -fimf-use-svml=true -qopenmp -DMYREAL8 -ftz  -extend-source -assume buffered_io -align array64byte -DMPI -DNO_SERIALIZE -I/opt/intel/oneapi/mpi/2021.11/include/    -DCPP_mach=CPP_p4linux964 -DCPP_rel=00  -g -traceback -c ${root_dir}/source/interfaces/intsort/inter7_filter_cand.F -o ./inter7_filter_cand.F.o

ifort  -I${root_dir}/../common_source/includes -I${root_dir}/share/includes -I${root_dir}/share/r8 -I${root_dir}/share/spe_inc  -module ./  -axSSE3,COMMON-AVX512 -no-fma -O3 -fp-model precise -fimf-use-svml=true -qopenmp -DMYREAL8 -ftz  -extend-source -assume buffered_io -align array64byte -DMPI -DNO_SERIALIZE -I/opt/intel/oneapi/mpi/2021.11/include/    -DCPP_mach=CPP_p4linux964 -DCPP_rel=00  -g -traceback -c ${root_dir}/source/interfaces/intsort/inter7_candidate_pairs.F -o ./inter7_candidate_pairs.F.o

ifort  -I${root_dir}/../common_source/includes -I${root_dir}/share/includes -I${root_dir}/share/r8 -I${root_dir}/share/spe_inc  -module ./  -axSSE3,COMMON-AVX512 -no-fma -O3 -fp-model precise -fimf-use-svml=true -qopenmp -DMYREAL8 -ftz  -extend-source -assume buffered_io -align array64byte -DMPI -DNO_SERIALIZE -I/opt/intel/oneapi/mpi/2021.11/include/    -DCPP_mach=CPP_p4linux964 -DCPP_rel=00  -g -traceback -c ${root_dir}/unit_test/unit_test1.F -o unit_test1.F.o

ifort -static-intel -rdynamic -qopenmp -qopenmp-link=static *.o  -o unit_test1   -L/opt/intel/oneapi/compiler/2024.0/lib/clang/17/lib/x86_64-unknown-linux-gnu  -ldl -ldl -lrt -lstdc++ 


