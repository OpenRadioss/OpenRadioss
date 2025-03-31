include_path=$(gcc -print-file-name=plugin/include)
g++ -fPIC -shared  -fno-rtti -I${include_path} -o fortran_signatures.so fortran_signatures.cpp
