# Set default compiler
COMP ?= gnu


ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
EXES=poc_openmp_$(COMP)
OBJDIR=objs
SRC=source
INCDIR=./source/include


ifeq ($(COMP),gnu)
CXX=g++
FC=gfortran

CXXFLAGS=-fopenmp -O2 -std=c++11
FCINC=-I$(ROOT_DIR)/${INCDIR} 
FCFLAGS= -J $(OBJDIR)/ -O2 -g -fbacktrace -march=native -fopenmp -DMYREAL8 -ffast-math -finit-real=nan -falign-commons -ffixed-line-length-132
LDFLAGS=
LIBS=-lstdc++
endif

ifeq ($(COMP),intel)
CXX=icpx
FC=ifx
INCDIR=./source/include
CXXFLAGS=-fiopenmp -O2 -std=c++11 -fopenmp-version=52
FCINC=-I$(ROOT_DIR)/${INCDIR} -module $(OBJDIR)
FCFLAGS=-O2 -g -traceback -axSSE3,COMMON-AVX512 -no-fma -fp-model precise -fimf-use-svml=true -qopenmp -DMYREAL8 -ftz -extend-source -assume buffered_io -align array64byte
LDFLAGS=
LIBS=-lstdc++
endif


ifeq ($(COMP),nvfortran)
CXX=pgc++
FC=nvfortran
INCDIR=./source/include
CXXFLAGS=-mp -O2 -std=c++11
FCINC=-I$(ROOT_DIR)/${INCDIR} -module $(OBJDIR)
FCFLAGS=-O2 -g -traceback -Minfo=all -mp -Mextend -DMYREAL8 -fast -Mfixed -Mallocatable=03
LDFLAGS=
LIBS=-lstdc++
endif



all: $(OBJDIR)/ $(EXES)
	

OBJS=$(OBJDIR)/compare_cand.o \
     $(OBJDIR)/unlimit_stack.o \
     $(OBJDIR)/constant_mod.o \
     $(OBJDIR)/inter7_filter_cand.o \
     $(OBJDIR)/inter7_penetration.o \
     $(OBJDIR)/inter7_candidate_pairs.o \
     $(OBJDIR)/inter7_gather_cand.o \
     $(OBJDIR)/collision_mod.o \
     $(OBJDIR)/unit_test1.o

$(OBJDIR)/compare_cand.o: $(SRC)/compare_cand.cpp

$(OBJDIR)/unlimit_stack.o: $(SRC)/unlimit_stack.cpp

$(OBJDIR)/constant_mod.o: $(SRC)/constant_mod.F \
        $(INCDIR)/my_real.inc

$(OBJDIR)/inter7_filter_cand.o: $(SRC)/inter7_filter_cand.F \
        $(OBJDIR)/constant_mod.o \
        $(OBJDIR)/collision_mod.o \
        $(OBJDIR)/inter7_gather_cand.o \
        $(OBJDIR)/inter7_penetration.o \
        $(INCDIR)/my_real.inc

$(OBJDIR)/inter7_penetration.o: $(SRC)/inter7_penetration.F \
        $(OBJDIR)/constant_mod.o \
        $(OBJDIR)/collision_mod.o \
        $(INCDIR)/my_real.inc

$(OBJDIR)/inter7_candidate_pairs.o: $(SRC)/inter7_candidate_pairs.F \
        $(OBJDIR)/constant_mod.o \
        $(OBJDIR)/collision_mod.o \
        $(OBJDIR)/inter7_filter_cand.o \
        $(INCDIR)/my_real.inc

$(OBJDIR)/inter7_gather_cand.o: $(SRC)/inter7_gather_cand.F \
        $(OBJDIR)/collision_mod.o \
        $(INCDIR)/my_real.inc

$(OBJDIR)/collision_mod.o: $(SRC)/collision_mod.F

$(OBJDIR)/unit_test1.o: $(SRC)/unit_test1.F \
        $(OBJDIR)/inter7_candidate_pairs.o


$(OBJDIR)/%.o: $(SRC)/%.F | $(OBJDIR)/
	$(FC) $(FCINC) $(FCFLAGS) -I$(INCDIR) -c $< -o $@

$(OBJDIR)/%.o: $(SRC)/%.cpp | $(OBJDIR)/
	$(CXX) $(CXXFLAGS) -I$(INCDIR) -c $< -o $@

$(EXES): $(OBJS)
	$(FC) $(FCFLAGS) $^ -o $@ $(LDFLAGS) $(LIBS)


.PHONY: clean all

objs/:
	mkdir -p objs

clean:
	-/bin/rm -rf objs $(EXES)

