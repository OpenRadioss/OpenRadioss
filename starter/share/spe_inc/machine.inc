Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
C
C----------------------------------------------------------------------
C
#if CPP_mach == CPP_p4win64
      CPUNAM ='win64'
      ARCHTITLE='Windows 64 bits, Intel compiler'
      IBUILTIN=22
C
#elif CPP_mach == CPP_linux964
      CPUNAM ='pgi_linux64'
      ARCHTITLE='Linux 64 bits, PGI compiler'
      IBUILTIN=12
C
#elif CPP_mach == CPP_p4linux964_spmd || CPP_mach == CPP_p4linux964
      CPUNAM = 'linux64'
#if CPP_rel == 70
      ARCHTITLE='Linux 64 bits, ARM compiler'
#elif CPP_rel == 80
      ARCHTITLE='Linux 64 bits, GNU compiler'
#elif CPP_rel == 96
      ARCHTITLE='Linux 64 bits, AMD AOCC compiler'
#elif 1
      ARCHTITLE='Linux 64 bits, Intel compiler'
#endif
      IBUILTIN=18
C
#elif 1
C (elif 1) remplace (else) qui ne marche pas sur sgi6 avec des (elif)
      DATA CPUNAM /'UNKNOWN PLATFORM'/
      ARCHTITLE='UNKNOWN PLATFORM'
      IBUILTIN = 1
#endif
C     NSPMD=NOMBRE DE PROCESSEURS SPMD
C     NSPMD=1 AUTREMENT
      NSPMD=1
C-----------------------
C     options internes
C-----------------------
C      BMUL0 = 0.25
      BMUL0 = 0.20     ! nouveau bumult agissant sur la marge
      IEXPM=1
C     IEXPM=1  allocation dynamique de memoire
C     IEXPM=0  dimension fixe

      ICRAY=0
      IF (IR4R8 .EQ. 1) THEN
        IRESP = 1
      ELSE
        IRESP = 0
      ENDIF

C     ICRAY=0  non cray
C     ICRAY=1  cray (hpalloc)
      IMACH=3
C     IMACH=0,1 ou 2  non spmd
C     IMACH=3  seule reste spmd
      IRFORM=12
C IRFORM= 5*irfe + irfl
C irfl=0  lecture fichiers de restart binaire 32/64 bits
C irfl=1  lecture ascii 
C irfl=2  lecture ieee 64 bits
C irfl=3  lecture ieee.Z 
C irfl=4  lecture ieee.gz 

C irfe=0  ecriture binaire
C irfe=1  ecriture ascii
C irfe=2  ecriture ieee 64 bits
C irfe=3  ecriture ieee.Z
C irfe=4  ecriture ieee.gz
C------------------------------------------------------------------
      IF (IRESP==1)THEN
        CPUNAM=CPUNAM(1:LEN_TRIM(CPUNAM))//" sp"
      ENDIF
C------------------------------------------------------------------
