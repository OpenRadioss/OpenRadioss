//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include <checksum.h>

std::string  checksum::compute_checksum(std::string file){
    std::ifstream input_file(file, std::ios::binary);

    if (!input_file) {
        std::cerr << "Error opening file: " << file << std::endl;
        checksum_list.push_back(std::make_tuple(file, "XXXXXXXXXXXXXXXX"));
        return "XXXXXXXXXXXXXXXX";
    }

    md5_state_t state;
    md5_init(&state);

    char buffer[BUFFERSIZE];
    int cont=1;

    while ( cont == 1){
        input_file.read(buffer, BUFFERSIZE);
        int count = input_file.gcount();
        md5_append(&state, ( md5_byte_t *)buffer, count);
        if ( count < BUFFERSIZE){
            cont=0;
        }   
    }
    input_file.close();

    unsigned char digest[16];
    md5_finish(&state, digest);

    std::ostringstream formatted_line;
        for (int i = 0; i < 16; ++i) {
           formatted_line << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(digest[i]);
        }
    checksum_list.push_back(std::make_tuple(file, formatted_line.str()));
    return formatted_line.str();
}


std::list <std::tuple< std::string, std::string>>  checksum::dump_list() {
    std::list <std::tuple< std::string, std::string>>  checksum_list_tmp;
    for (const auto& entry : checksum_list) {
        checksum_list_tmp.push_back(entry);
    }
    return checksum_list_tmp;
}


// C/FORTRAN Engine interface
extern "C" {
    checksum* new_file_checksum_list(){
         checksum* cs_output_files = new checksum();
         return cs_output_files;
    }

    void compute_binary_checksum(checksum* cs_output_files,char *file, int len , int izip) {
        char *file_c;
        file_c=(char*)malloc(sizeof(char)* len+1);
        for (int i=0; i<len; i++){
            file_c[i]=file[i];
        }
        file_c[len]='\0';

        std::string file_str(file_c);
        if (izip > 0) {
            file_str = file_str + ".gz";
        }   

        std::string checksum_str=cs_output_files->compute_checksum(file_str);
        // std::cout << "Checksum for file " << file_str << " is: " << checksum_str << std::endl;
    }

    void print_checksum_list( checksum* cs_output_files,int fd) {
        std::list<std::tuple<std::string, std::string>> checksum_list = cs_output_files->dump_list();
         for (const auto& entry : checksum_list) {
            std::string str_line="    " + std::get<0>(entry) + " " + std::get<1>(entry); 
            int len_line=str_line.length();
            const char * c_line =  str_line.c_str();
            write_out_file(&fd,c_line,&len_line);
        }
    }

    
}

#ifdef MAIN

int main(int argc, char *argv[]) {
    checksum cs;
    std::string file=std::string(argv[1]);
    std::string checksum = cs.compute_checksum(file);
    std::cout << "Checksum for file " << file << " is: " << checksum << std::endl;
    return 0;
}
#endif