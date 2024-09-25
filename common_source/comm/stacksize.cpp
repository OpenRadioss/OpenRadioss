//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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
#include <cstdlib>
#include <iostream>
#include <string>
#include <cstring>

#ifdef _WIN64
// Windows only
#include <Windows.h>
#include <processthreadsapi.h>
#else
// Linux only
#include <stdio.h>
#include <sys/resource.h>
#endif

#ifdef _OPENMP
#include <omp.h>
#endif

#define _FCALL

char omp_stacksize[128];
char stacksize[128];

void solver_stacksize();


// -------------------------------------------------------------------------------------------------------
// Solver_Stacksize
// -------------------------------------------------------------------------------------------------------
// Gets the defined OMP_STACKSIZE or KMP_STACKSIZE for the solver
// For intel Compilers : if less than 250MB, set to 250MB
// For other compilers (gfortran / armflang): get the value from the environment variable OMP_STACKSIZE
// check it it makes sense. store the value in the global variable omp_stacksize
//
// Get the stacksize of the solver
// -------------------------------------------------------------------------------------------------------
void solver_stacksize(){

// -------------------------------------------------------------------------------------------------------
// Thread Stacksize : OMP_STACKSIZE or KMP_STACKSIZE
// -------------------------------------------------------------------------------------------------------
std::string str_stack;

#ifdef _OPENMP

#if defined(__INTEL_COMPILER) || defined(__INTEL_LLVM_COMPILER)

   // Intel Compilers use kmp_get_stacksize / kmp_set_stacksize API
   size_t stack_size = kmp_get_stacksize();
   if (stack_size < 262144000){          // < 250 MB, set to 250 MB 
      stack_size = 262144000;
      kmp_set_stacksize(stack_size);
   }
   size_t stack_mb = stack_size /1048576;
   str_stack = std::to_string(stack_mb)+" MB";

#else

   // No API on gfortran or ArmFlang
   // Grab the value from the environment variable OMP_STACKSIZE and postrpocess it
   // tp have a value in MB or K,M,G if expresed like this
   const char* env_stack_size = std::getenv("OMP_STACKSIZE");

   if ( env_stack_size == NULL) {
       str_stack = "0 MB";

   }else{
       char last_char = env_stack_size[strlen(env_stack_size)-1];

       if (last_char == 'G' || last_char == 'g' || last_char == 'M' || last_char == 'm' || last_char == 'K' || last_char == 'k'){   // Value is set in KB or MB or GB
           char *stack_size = strdup(env_stack_size);
           stack_size[strlen(stack_size)-1] = '\0';
       try{                                               // Try to convert value in Integer - if Rubish print 0 as OMP_STACKSIZE
           int stack_size_int = std::stoi(stack_size);
           str_stack = std::to_string(stack_size_int)+" "+last_char+"B";
       }
       catch (...){                                      // Cannot convert 
           str_stack = "0 MB";
       }
  }else{                                             // Value is set in KBytes / Read & tey to convert in MB
    try{
       int stack_size_int = std::stoi(env_stack_size)/1024;
       str_stack = std::to_string(stack_size_int)+" MB";
    }
    catch (...){
        str_stack = "0 MB";
    }
  }
}

#endif

#else 
str_stack = std::string("0");
#endif

// -------------------------------------------------------------------------------------------------------
// Stacksize on Windows or Linux
// -------------------------------------------------------------------------------------------------------
#ifdef _WIN64
// Stacksize on Windows with Handle
// -------------------------------------------------------------------------------------------------------
    std::string stsize;

    // Method : Get the stacksize from the PE Header
    // Grab in the Header from Module Handle
    HMODULE Program= GetModuleHandle(NULL);
    if(Program != NULL) {
       IMAGE_DOS_HEADER* pDOSHeader = (IMAGE_DOS_HEADER*)Program;
       IMAGE_NT_HEADERS* pNTHeaders =(IMAGE_NT_HEADERS*)((BYTE*)pDOSHeader + pDOSHeader->e_lfanew);
       PIMAGE_OPTIONAL_HEADER pOptionalHeader = &pNTHeaders->OptionalHeader;
       DWORD stack=pOptionalHeader->SizeOfStackReserve /(1024*1024);
       stsize = std::to_string(stack) + " MB";
    }else{
        stsize = "-1 MB";
    }
        strcpy_s(stacksize, 128, stsize.c_str());
#else
// Stacksize on Linux getrlimit call
// -------------------------------------------------------------------------------------------------------
        struct rlimit rl;
        int result = getrlimit(RLIMIT_STACK, &rl);
        if (result == 0){
                if (rl.rlim_cur == -1){
                    strcpy(stacksize, "Unlimited");
                }else{
                    long int StMB = rl.rlim_cur / (1024*1024);
                    sprintf(stacksize, "%ld MB", StMB);
                    // Try to increase Stacksize
                    rl.rlim_cur = rl.rlim_max;
                    setrlimit(RLIMIT_STACK, &rl);
                }
        }else{
            strcpy(stacksize, "-1 MB");
        }
#endif

#ifdef _WIN64
        strcpy_s(omp_stacksize, 128, str_stack.c_str());
#else
        strcpy(omp_stacksize, str_stack.c_str());
#endif
}

extern "C" {
    // get solver stacksize : return the stacksize stored in the global variable omp_stacksize
    // stacksize : output string must be allocted
    // len : length of the output string : must be at least 128
    void get_solver_stacksize(char* stsize, int* stsize_len, char *omp_stsize,int * omp_stsize_len){
#ifdef _WIN64
        strcpy_s(stsize, *stsize_len, stacksize);
        strcpy_s(omp_stsize, *omp_stsize_len, omp_stacksize);

#else
        strcpy(stsize, stacksize);
        strcpy(omp_stsize, omp_stacksize);
#endif
        *stsize_len = strlen(stacksize);
        *omp_stsize_len = strlen(omp_stacksize);
    }


    void get_solver_stacksize_(char* stsize, int* stsize_len, char *omp_stsize,int * omp_stsize_len){
#ifdef _WIN64
        strcpy_s(stsize, *stsize_len, stacksize);
        strcpy_s(omp_stsize, *omp_stsize_len, omp_stacksize);

#else
        strcpy(stsize, stacksize);
        strcpy(omp_stsize, omp_stacksize);
#endif
        *stsize_len = strlen(stacksize);
        *omp_stsize_len = strlen(omp_stacksize);
    }

    void get_solver_stacksize__(char* stsize, int* stsize_len, char *omp_stsize,int * omp_stsize_len){
#ifdef _WIN64
        strcpy_s(stsize, *stsize_len, stacksize);
        strcpy_s(omp_stsize, *omp_stsize_len, omp_stacksize);

#else
        strcpy(stsize, stacksize);
        strcpy(omp_stsize, omp_stacksize);
#endif
        *stsize_len = strlen(stacksize);
        *omp_stsize_len = strlen(omp_stacksize);
    }

    void _FCALL GET_SOLVER_STACKSIZE(char* stsize, int* stsize_len, char *omp_stsize,int * omp_stsize_len){
#ifdef _WIN64
        strcpy_s(stsize, *stsize_len, stacksize);
        strcpy_s(omp_stsize, *omp_stsize_len, omp_stacksize);

#else
        strcpy(stsize, stacksize);
        strcpy(omp_stsize, omp_stacksize);
#endif
        *stsize_len = strlen(stacksize);
        *omp_stsize_len = strlen(omp_stacksize);
    }


    void solver_stacksize_(){ 
        solver_stacksize();
    }

    void solver_stacksize__(){ 
       solver_stacksize();
    }

    void _FCALL SOLVER_STACKSIZE(){
       solver_stacksize();
    }

}

#ifdef USE_MAIN

int main(){
    solver_stacksize();
    int  stsize_len;
    char stsize[128];
    int omp_stsize_len;
    char omp_stsize[128];

    stsize_len=128;
    omp_stsize_len=128;
    get_solver_stacksize(stsize, &stsize_len, omp_stsize, &omp_stsize_len);

    std::cout << "Stacksize: " << stsize << std::endl;
    std::cout << "OMP_STACKSIZE: " << omp_stsize << std::endl;

    HMODULE Program= GetModuleHandle(NULL);
    if(Program == NULL) return 0;
    IMAGE_DOS_HEADER* pDOSHeader = (IMAGE_DOS_HEADER*)Program;
    IMAGE_NT_HEADERS* pNTHeaders =(IMAGE_NT_HEADERS*)((BYTE*)pDOSHeader + pDOSHeader->e_lfanew);
    PIMAGE_OPTIONAL_HEADER pOptionalHeader = &pNTHeaders->OptionalHeader;

    std::cout << "yeah!!!" << pNTHeaders->OptionalHeader.SizeOfImage << std::endl;
    std::cout << "SizeOfStackReserve: " << std::hex << pOptionalHeader->SizeOfStackReserve << std::endl;
    std::cout << "SizeOfStackReserve: " << std::dec << pOptionalHeader->SizeOfStackReserve << std::endl;

    exit(0);
}

#endif

