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
Cel50c42+++
Chd|====================================================================
Chd|  SPHBOX                        share/modules/sphbox.F        
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|        SOLTOSPH_ON1                  source/elements/sph/soltosph_on1.F
Chd|        SOLTOSPH_ON12                 source/elements/sph/soltosph_on1.F
Chd|        SOLTOSPH_ON2                  source/elements/sph/soltosph_on2.F
Chd|        SPADASM                       source/elements/sph/spadasm.F 
Chd|        SPADASM0                      source/elements/sph/spadasm.F 
Chd|        SPBUC3                        source/elements/sph/spbuc3.F  
Chd|        SPCLASV                       source/elements/sph/spclasv.F 
Chd|        SPCOMPL                       source/elements/sph/spcompl.F 
Chd|        SPDENS                        source/elements/sph/spdens.F  
Chd|        SPFORCP                       source/elements/sph/spforcp.F 
Chd|        SPGAUGE                       source/elements/sph/spgauge.F 
Chd|        SPGRADT                       source/elements/sph/sptemp.F  
Chd|        SPGTSYM                       source/elements/sph/sptemp.F  
Chd|        SPHTRI                        source/elements/sph/sphtri.F  
Chd|        SPLAPLT                       source/elements/sph/sptemp.F  
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|        SPMD_INISPH                   source/mpi/sph/spmd_sptool.F  
Chd|        SPMD_SAVESPH                  source/mpi/sph/spmd_sptool.F  
Chd|        SPMD_SPAMAJ                   source/mpi/sph/spmd_spamaj.F  
Chd|        SPMD_SPAMAJ_old               source/mpi/sph/spmd_spamaj.F  
Chd|        SPMD_SPHBOX                   source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGAT                   source/mpi/sph/spmd_sphgat.F  
Chd|        SPMD_SPHGAT_old               source/mpi/sph/spmd_sphgat.F  
Chd|        SPMD_SPHGETA                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETD                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETDK                 source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETF                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETG                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETH                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETIMP                source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETISPH               source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETSTB                source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETT                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETV                  source/mpi/sph/spmd_sphgetv.F 
Chd|        SPMD_SPHGETVOIS_OFF           source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETV_OLD              source/mpi/sph/spmd_sphgetv.F 
Chd|        SPMD_SPHGETW                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETWA                 source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETX                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHVOX                   source/mpi/sph/spmd_sphvox.F  
Chd|        SPMD_SPHVOX_OLD               source/mpi/sph/spmd_sphvox.F  
Chd|        SPONFPRS                      source/elements/sph/sponfprs.F
Chd|        SPONFRO                       source/elements/sph/sponfro.F 
Chd|        SPONFV                        source/elements/sph/sponfv.F  
Chd|        SPONOF2                       source/elements/sph/sponof2.F 
Chd|        SPPRO3                        source/elements/sph/sppro3.F  
Chd|        SPSCOMP                       source/elements/sph/spcompl.F 
Chd|        SPSGSYM                       source/elements/sph/spsgsym.F 
Chd|        SPSTABS                       source/elements/sph/spstab.F  
Chd|        SPSTABW                       source/elements/sph/spstab.F  
Chd|        SPSYMP                        source/elements/sph/spsym.F   
Chd|        SPSYM_ALLOC                   source/elements/sph/spsym_alloc.F
Chd|        SPTRIVOX                      source/elements/sph/sptrivox.F
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SPHBOX
#include "my_real.inc"
      TYPE real_pointer5
        my_real, DIMENSION(:), POINTER :: P
      END TYPE real_pointer5
C nombre de particules sph remote totale a recevoir et a envoyer
      INTEGER NSPHR, NSPHS
C dimension vecteur de stockage partie tri , calcul et wacomp
      INTEGER, PARAMETER :: SIZSPT = 6, SIZSPC = 14, SIZSPW = 16
C flags particule active cote recu et envoyee
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISPHR, ISPHS
C nombre de cellule a recevoir/envoyer par processeur
      INTEGER, DIMENSION(:), ALLOCATABLE :: PSPHR, PSPHS
C liste triee par processeur des cellules locales a renvoyer aux autres processeurs
      INTEGER, DIMENSION(:), ALLOCATABLE :: LSPHS
C tableau particules symetrique par plan de symetrie
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISPSYMR
C tableau cell remote pour sph
      my_real       ,DIMENSION(:,:), ALLOCATABLE :: XSPHR
C DVOIS(KVOISPH) des particules remote, dans le cas ou elles ont subit
C une reduction 
      my_real       ,DIMENSION(:), ALLOCATABLE :: DKR, DKS 
C XSPHR(1) :  no local N sur proc remote
C XSPHR(2) :  h
C XSPHR(3) :  X(1)
C XSPHR(4) :  X(2)
C XSPHR(5) :  X(3)
C XSPHR(6) :  ID particule
C XSPHR(7) :  RHO
C XSPHR(8) :  Mass of the particle (rho*vol)
C XSPHR(9) :  V(1)
C XSPHR(10) : V(2)
C XSPHR(11) : V(3)
C XSPHR(12) : KXSP(2,N)/(NGROUP+1)
C XSPHR(13) : KXSP(2,N)
C XSPHR(14) : IPARTSP(N)
C tableau additionnel cell remote pour sph (wacomp)
      my_real       ,DIMENSION(:,:), ALLOCATABLE :: WACOMPR
C tableau additionnel cell remote pour sph (DSPHR)
c utilise pour traitement IPPV remotes dans sponfv
c      my_real       ,DIMENSION(:,:), ALLOCATABLE :: DSPHR
c DSPHR(1) : DXX
c DSPHR(2) : DXY
c DSPHR(3) : DXZ
c DSPHR(4) : DYX
c DSPHR(5) : DYY
c DSPHR(6) : DYZ
c DSPHR(7) : DZX
c DSPHR(8) : DZY
c DSPHR(9) : DZZ
c DSPHR(10) : VX
c DSPHR(11) : VY
c DSPHR(12) : VZ
C dim1 WACOMPR = dim1 WACOMP = SIZSPW

      TYPE SPSYM_STRUCT
        my_real, DIMENSION(:)  , ALLOCATABLE :: BUF 
      END TYPE SPSYM_STRUCT

      END MODULE SPHBOX
Cel50c42---
