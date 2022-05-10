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
C
      ISHF=0
C     ISHF=0 shear factor =5/6
C     ISHF=1 pseudo shear factor (plus stable et moins rigide)
      NCINP=10
C     NCINP=1
C     NCINP nombre de cycles entre la lecture du fichier de commande rootCnn
#if CPP_mach == CPP_c90 || CPP_mach == CPP_ymp || CPP_mach == CPP_ymp_spmd || CPP_mach == CPP_t90 || CPP_mach == CPP_t90_i3e
      ICRAY=1
      IRESP = 0
#else
      ICRAY=0
      IF (IR4R8 .EQ. 1) THEN
        IRESP = 1
      ELSE
        IRESP = 0
      ENDIF
#endif
#if CPP_mach == CPP_c90 || CPP_mach == CPP_ymp || CPP_mach == CPP_ymp_spmd || CPP_mach == CPP_t90 || CPP_mach == CPP_t90_i3e || CPP_mach == CPP_sx4 || CPP_mach == CPP_sx4_i3e || CPP_mach == CPP_vpp || CPP_mach == CPP_vpp_spmd || CPP_mach == CPP_sx5_spmd  || CPP_mach == CPP_cx1 || CPP_mach == CPP_cx1_spmd
      IVECTOR = 1
#else
      IVECTOR = 0
#endif
C#if defined(PVM) || defined(MPI)
      IMACH=3
C#else
C      IMACH=0
c#endif
C     IMACH=0,1 ou 2  non spmd
C     IMACH=3  spmd
      NTHREAD=1
C     NTHREAD=1 un seul processeur
C     NTHREAD=n n processeurs
      IEXPM=1
C     IEXPM=0  standard
C     IEXPM=1  allocation dynamique
      IGFORM=3
C     IGFORM=0  fichiers de dessin binaire
C     IGFORM=1  fichiers de dessin code
C     IGFORM=3  fichiers de dessin IEEE
      ITFORM=3
C     ITFORM=0  fichiers de T. H. binaire
C     ITFORM=1  fichiers de T. H. code
C     ITFORM=2  fichiers de T. H. formates (I5,1PE15.8)
C     ITFORM=3  fichiers de T. H. IEEE
      IRFORM=12
C IRFORM=0  fichiers de restart binaire 32 bits
C IRFORM=1  lecture ascii + ecriture binaire
C IRFORM=2  lecture ieee + ecriture binaire
C IRFORM=3  lecture ieee.Z + ecriture binaire
C IRFORM=4  lecture ieee.gz + ecriture binaire

C IRFORM=5  lecture binaire + ecriture ascii
C IRFORM=6  lecture ascii + ecriture ascii
C IRFORM=7  lecture ieee + ecriture ascii
C IRFORM=8  lecture ieee.Z + ecriture ascii
C IRFORM=9  lecture ieee.gz + ecriture ascii

C IRFORM=10  lecture binaire + ecriture ieee
C IRFORM=11  lecture ascii + ecriture ieee
C IRFORM=12  lecture ieee + ecriture ieee
C IRFORM=13  lecture ieee.Z + ecriture ieee
C IRFORM=14  lecture ieee.gz + ecriture ieee

C IRFORM=15  lecture binaire + ecriture ieee.Z
C IRFORM=16  lecture ascii + ecriture ieee.Z
C IRFORM=17  lecture ieee + ecriture ieee.Z
C IRFORM=18  lecture ieee.Z + ecriture ieee.Z
C IRFORM=19  lecture ieee.gz + ecriture ieee.Z

C IRFORM=20  lecture binaire + ecriture ieee.gz
C IRFORM=21  lecture ascii + ecriture ieee.gz
C IRFORM=22  lecture ieee + ecriture ieee.gz
C IRFORM=23  lecture ieee.Z + ecriture ieee.gz
C IRFORM=24  lecture ieee.gz + ecriture ieee.gz
