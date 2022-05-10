Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
#include "hardware.inc"
C----------------------------------------------------------------------
C ALLOC
      my_real
     .  , DIMENSION(:), ALLOCATABLE :: AF
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAF
      REAL (KIND=KIND(1.D0)), DIMENSION(:), ALLOCATABLE :: XDP
      INTEGER IADI,IADR,IXDP,IADIF,IADRF
C----------------------------------------------------------------------
C
#if CPP_mach == CPP_p4win32
      CPUNAM ='win32'
      ARCHTITLE='Windows 32 bits, Intel compiler'
C
#elif CPP_mach == CPP_p4win64
#if CPP_rel == 10
      CPUNAM ='win64 plmpi'
      ARCHTITLE='Windows 64 bits, Intel compiler, Platform MPI'
#elif CPP_rel == 20
      CPUNAM ='win64 impi'
      ARCHTITLE='Windows 64 bits, Intel compiler, Intel MPI'
#elif CPP_rel == 30
      CPUNAM ='win64 msmpi'
      ARCHTITLE='Windows 64 bits, Intel compiler, MS MPI'
#elif 1
      CPUNAM ='win64'
      ARCHTITLE='Windows 64 bits, Intel compiler'
#endif
C
#elif CPP_mach == CPP_linux
      CPUNAM ='linux32 pgi'
      ARCHTITLE='Linux 32 bits, PGI compiler'
C
#elif CPP_mach == CPP_p4linux932
      CPUNAM ='linux32'
      ARCHTITLE='Linux 32 bits, Intel compiler'
C
#elif CPP_mach == CPP_linux964
      CPUNAM ='pgi_linux64'
      ARCHTITLE='Linux 64 bits, PGI compiler'
C
#elif CPP_mach == CPP_linux64_spmd
#if CPP_rel == 60
      CPUNAM='linux64 pgi plmpi'
      ARCHTITLE='Linux 64 bits, PGI compiler, Platform MPI'
#endif
C
#elif CPP_mach == CPP_p4linux964_spmd || CPP_mach == CPP_p4linux964
#if CPP_rel==20
      CPUNAM='linux64 plmpi'
      ARCHTITLE='Linux 64 bits, Intel compiler, Platform MPI'
#elif CPP_rel == 30
      CPUNAM='linux64 bmpi'
      ARCHTITLE='Linux 64 bits, Intel compiler, BullxMPI'
#elif CPP_rel == 40
      CPUNAM='linux64 bmpi'
      ARCHTITLE='Linux 64 bits, Intel compiler, OpenMPI'
#elif CPP_rel == 70
#if defined(MPI)
        CPUNAM='linuxa64 ompi'
        ARCHTITLE='Linux 64 bits, ARM compiler, OpenMPI'
#elif 1
        CPUNAM='linuxa64'
        ARCHTITLE='Linux 64 bits, ARM compiler'
#endif
#elif CPP_rel == 80
#if defined(MPI)
        CPUNAM='linuxa64 gf ompi'
        ARCHTITLE='Linux 64 bits, GNU compiler, OpenMPI'
#elif 1
        CPUNAM='linuxa64 gf'
        ARCHTITLE='Linux 64 bits, GNU compiler'
#endif

#elif 1
#if defined(MPI)
      CPUNAM = 'linux64 impi'
      ARCHTITLE='Linux 64 bits, Intel compiler, Intel MPI'
#elif 1
      CPUNAM = 'linux64'
      ARCHTITLE='Linux 64 bits, Intel compiler'
#endif
#endif
C
#elif CPP_mach == CPP_il
      CPUNAM = 'linuxia64'
      ARCHTITLE='Linux Itanium 64 bits, Intel compiler'
C
#elif CPP_mach == CPP_il_spmd
#if CPP_rel == 10
      CPUNAM = 'linuxia64 plmpi'
      ARCHTITLE='Linux Itanium 64 bits, Intel compiler, Platform MPI'
#elif CPP_rel == 30
      CPUNAM = 'linuxia64 sgimpi'
      ARCHTITLE='Linux Itanium 64 bits, Intel compiler, SGI MPI'
#endif
C
#elif CPP_mach == CPP_pwr4 ||  CPP_mach == CPP_pwr4_spmd
#if defined(MPI)
      CPUNAM ='aix64 aixmpi'
      ARCHTITLE='AIX 64 bits, IBM compiler, POE MPI'
C
#elif 1
      CPUNAM ='aix64'
      ARCHTITLE='AIX 64 bits, IBM compiler'
#endif
C
#elif CPP_mach == CPP_sun25
#if CPP_rel == 1000
      CPUNAM ='solarisx64'
      ARCHTITLE='Solaris 64 bits, SUN compiler'
#endif
C
#elif CPP_mach == CPP_sol10x64_spmd
      CPUNAM ='solarisx64 sunmpi'
      ARCHTITLE='Solaris 64 bits, SUN compiler, SUN MPI'
C
#elif CPP_mach == CPP_macosx64
      CPUNAM ='macosx'
      ARCHTITLE='Mac OS/X 64 bits, Intel compiler'
#elif 1
C (elif 1) remplace (else) qui ne marche pas sur sgi6 avec des (elif)
      DATA CPUNAM /'UNKNOWN PLATFORM'/
      ARCHTITLE='UNKNOWN PLATFORM'
#endif
      IF (IR4R8==1)THEN
        CPUNAM=CPUNAM(1:LEN_TRIM(CPUNAM))//" sp"
      ENDIF

