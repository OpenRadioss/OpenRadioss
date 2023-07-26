current_path=${PWD}
v_lapack=3.10.1
v_scalapack=2.2.0
v_mumps=5.5.1

wget https://github.com/Reference-LAPACK/lapack/archive/refs/tags/v${v_lapack}.tar.gz
wget https://github.com/Reference-ScaLAPACK/scalapack/archive/refs/tags/v${v_scalapack}.tar.gz 
wget http://ftp.mcs.anl.gov/pub/petsc/externalpackages/MUMPS_${v_mumps}.tar.gz
#
tar -xvf v${v_lapack}.tar.gz
tar -xvf v${v_scalapack}.tar.gz
tar -xvf MUMPS_${v_mumps}.tar.gz

cd lapack-${v_lapack}
cp make.inc.example make.inc
sed -i 's/^FORTRAN.*/FORTRAN  = gfortran/g' make.inc
sed -i 's/^OPTS.*/OPTS     = -O2 -frecursive -fallow-argument-mismatch/g' make.inc
sed -i 's/^LOADER.*/LOADER   = gfortran/g' make.inc
make blaslib
make lapacklib
make tmglib

cd ../scalapack-${v_scalapack}
mkdir build
cd build
cmake .. -DBUILD_SHARED_LIBS=off -DCMAKE_Fortran_COMPILER=mpif90 -DCMAKE_INSTALL_PREFIX=./install -DBLAS_LIBRARIES=${current_path}/lapack-${v_lapack}/librefblas.a -DLAPACK_LIBRARIES=${current_path}/lapack-${v_lapack}/liblapack.a -DCMAKE_Fortran_FLAGS="-fallow-argument-mismatch" 
make
make install
cd ../../


cd MUMPS_${v_mumps}


echo 'LPORDDIR = $(topdir)/PORD/lib/' > Makefile.inc
echo 'IPORD    = -I$(topdir)/PORD/include/' >> Makefile.inc
echo 'LPORD    = -L$(LPORDDIR) -lpord' >> Makefile.inc
echo 'ORDERINGSF  = -Dpord' >> Makefile.inc
echo 'ORDERINGSC  = $(ORDERINGSF)' >> Makefile.inc
echo 'LORDERINGS = $(LPORD)' >> Makefile.inc
echo 'IORDERINGSC = $(IPORD)'  >> Makefile.inc
echo 'LIBEXT  = .a' >> Makefile.inc
echo 'OUTC    = -o ' >> Makefile.inc
echo 'OUTF    = -o ' >> Makefile.inc
echo 'RM      = /bin/rm -f' >> Makefile.inc
echo 'CC      = mpicc' >> Makefile.inc
echo 'FC      = mpif90' >> Makefile.inc
echo 'FL      = mpif90' >> Makefile.inc
echo 'AR      = ar vr ' >> Makefile.inc
echo 'RANLIB  = ranlib' >> Makefile.inc
echo 'LAPACK=  -L$(topdir)/../'"lapack-${v_lapack}"' -llapack ' >> Makefile.inc
echo 'SCALAP  = -L$(topdir)/../scalapack-'"${v_scalapack}"'/build/lib -lscalapack ' >> Makefile.inc
echo 'BLAS=  -L$(topdir)/../lapack-'"${v_lapack}"' -lblas ' >> Makefile.inc
echo 'INCPAR  = -I/usr/include' >> Makefile.inc
echo 'LIBPAR  = $(SCALAP) $(LAPACK) -L/usr/lib -lmpi ' >> Makefile.inc
echo 'INCSEQ  = -I$(topdir)/libseq' >> Makefile.inc
echo 'LIBSEQ  = $(LAPACK) -L$(topdir)/libseq -lmpiseq' >> Makefile.inc
echo 'LIBBLAS = -L$(topdir)/../lapack-'"${v_lapack}"'/librefblas.a -lblas ' >> Makefile.inc
echo 'LIBOTHERS = -lpthread' >> Makefile.inc
echo 'CDEFS = -DAdd_' >> Makefile.inc
echo 'OPTF    = -O -fallow-argument-mismatch' >> Makefile.inc
echo 'OPTC    = -O -I.' >> Makefile.inc
echo 'OPTL    = -O' >> Makefile.inc
echo 'INCS = $(INCPAR)' >> Makefile.inc
echo 'LIBS = $(LIBPAR)' >> Makefile.inc
echo 'LIBSEQNEEDED = ' >> Makefile.inc

# Now you can build MUMPS
make all

#
rm *gz
rm *gz.1
rm *gz.2
