current_path=${PWD}
v_mumps=5.5.1
wget http://ftp.mcs.anl.gov/pub/petsc/externalpackages/MUMPS_${v_mumps}.tar.gz
tar -xvf MUMPS_${v_mumps}.tar.gz
rm *gz
rm *gz.1
rm *gz.2
