#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <list>
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include <string.h>
#include <tuple>
#include <filesystem>
#include <algorithm>

#include <md5.h>

#define _FCALL 
#define BUFFERSIZE 4096

class checksum {
private:
    std::list <std::tuple< std::string, std::string>> checksum_list;   // List of checksums : Filename, checksum
public:
    std::string compute_checksum(std::string file);
    std::list <std::tuple< std::string, std::string>> dump_list();
};


#ifdef _WIN64
#define write_out_file WRITE_OUT_FILE
#else
#define write_out_file write_out_file_
#endif

extern "C" {
    checksum* new_file_checksum_list();
    void compute_binary_checksum(checksum* cs_output_files,char *file, int len , int izip);
    void print_checksum_list( checksum* cs_output_files,int fd);
    void write_out_file(int * fd,const char * line,int * len_line);
}
