#!/usr/bin/env python3
import os
import fileinput
import re
import shutil

fortran_ext = ['.F']
cpp_ext = ['.h', '.cpp', '.c','.hpp','.cxx']
cfg_ext = ['.cfg']
starter_deck_ext = ['_0000.rad']
engine_deck_ext = ['_0001.rad']


def is_fortran(f):
    results = [f.endswith(ext) for ext in fortran_ext]
    return True in results

def is_cpp(f):
    results = [f.endswith(ext) for ext in cpp_ext]
    return True in results
def is_cfg(f):
    results = [f.endswith(ext) for ext in cfg_ext]
    return True in results

def is_starter_deck(f):
    results = [f.endswith(ext) for ext in starter_deck_ext]
    return True in results

def is_engine_deck(f):
    results = [f.endswith(ext) for ext in engine_deck_ext]
    return True in results


def apply_header():
    for root, dirs, files in os.walk("../../"):
        if (not re.search("/com/",root)) and (not re.search("/extlib/",root)) and (not re.search("CMake",root)):
            for filename in files:
                if is_fortran(filename): 
                    add_header(os.path.join(root,filename))
                elif is_cpp(filename):
                    add_header(os.path.join(root,filename))
                elif is_cfg(filename):
                    add_header(os.path.join(root,filename))
                elif is_starter_deck(filename):
                    add_header(os.path.join(root,filename))
                elif is_engine_deck(filename):
                    add_header(os.path.join(root,filename))


def add_header(filename):
    if is_fortran(filename):
        fic = "f_COPYRIGHT.txt"
    elif is_cpp(filename):
        fic = "cpp_COPYRIGHT.txt"
    elif is_cfg(filename):
        fic = "cfg_COPYRIGHT.txt"
    elif is_starter_deck(filename):
        fic = "rad_COPYRIGHT.txt"
    elif is_engine_deck(filename):
        fic = "rad_COPYRIGHT.txt"

    with open(fic) as f:
        nbl=len([0 for _ in f])     

#    print(filename)
# check if the header is correct
    with open(filename,encoding='latin1') as f1, open(fic,encoding='latin1') as f2:
        ok_header = True
        # Starter Deck must Start with #RADIOSS
        if is_starter_deck(filename):
            line=f1.readline()

        for i in range(nbl):
            if f1.readline() != f2.readline():
                ok_header = False

#if it is not, then 
        if ok_header == False:
            print("WARNING: "+filename+" has no copyright")

            if is_starter_deck(filename):
                with open(filename,encoding='latin1') as f1, open(filename+".bak",'w',encoding='latin1') as f2:
                  # Fist line must be #RADIOSS STARTER, copy it
                  line=f1.readline()
                  f2.write(line) 
                  # Copy the License Header
                  with open(fic,encoding='latin1') as fl:
                     for line_lic in fl:
                        f2.write(line_lic)
                  
                  # Terminate Starter Deck
                  for line in f1:
                        f2.write(line)

                shutil.move(filename+".bak",filename)

            else:
                shutil.copy(fic,filename+".bak")
                with open(filename,encoding='latin1') as f1, open(filename+".bak",'a',encoding='latin1') as f2:
                    for line in f1:
                        if not re.search("Copyright>",line):
                            if is_cfg(filename):
                                    if ((not re.search("Copyright",line)) 
                                       and (not re.search("Contains trade secrets",line)) 
                                       and (not re.search("HWVERSION",line))):
                                            f2.write(line)
                            else:
                                    f2.write(line) 
                shutil.move(filename+".bak",filename)                        

#============================================================
apply_header()


