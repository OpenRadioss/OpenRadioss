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
      !||====================================================================
      !||    sphbox                ../engine/share/modules/sphbox.F
      !||--- called by ------------------------------------------------------
      !||    forintp               ../engine/source/elements/forintp.F
      !||    restmod               ../engine/share/modules/restart_mod.F
      !||    soltosph_on1          ../engine/source/elements/sph/soltosph_on1.F
      !||    soltosph_on12         ../engine/source/elements/sph/soltosph_on1.F
      !||    soltosph_on2          ../engine/source/elements/sph/soltosph_on2.F
      !||    spadasm               ../engine/source/elements/sph/spadasm.F
      !||    spadasm0              ../engine/source/elements/sph/spadasm.F
      !||    spbuc3                ../engine/source/elements/sph/spbuc3.F
      !||    spclasv               ../engine/source/elements/sph/spclasv.F
      !||    spcompl               ../engine/source/elements/sph/spcompl.F
      !||    spdens                ../engine/source/elements/sph/spdens.F
      !||    spforcp               ../engine/source/elements/sph/spforcp.F
      !||    spgauge               ../engine/source/elements/sph/spgauge.F
      !||    spgradt               ../engine/source/elements/sph/sptemp.F
      !||    spgtsym               ../engine/source/elements/sph/sptemp.F
      !||    sphtri                ../engine/source/elements/sph/sphtri.F
      !||    splaplt               ../engine/source/elements/sph/sptemp.F
      !||    splissv               ../engine/source/elements/sph/splissv.F
      !||    spmd_inisph           ../engine/source/mpi/sph/spmd_sptool.F
      !||    spmd_savesph          ../engine/source/mpi/sph/spmd_sptool.F
      !||    spmd_spamaj           ../engine/source/mpi/sph/spmd_spamaj.F
      !||    spmd_sphgat           ../engine/source/mpi/sph/spmd_sphgat.F
      !||    spmd_sphgeta          ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetd          ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetdk         ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetf          ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetg          ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgeth          ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetimp        ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetisph       ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetstb        ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgett          ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetv          ../engine/source/mpi/sph/spmd_sphgetv.F
      !||    spmd_sphgetvois_off   ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetw          ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetwa         ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetx          ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphvox           ../engine/source/mpi/sph/spmd_sphvox.F
      !||    sponfprs              ../engine/source/elements/sph/sponfprs.F
      !||    sponfro               ../engine/source/elements/sph/sponfro.F
      !||    sponfv                ../engine/source/elements/sph/sponfv.F
      !||    sponof2               ../engine/source/elements/sph/sponof2.F
      !||    sppro3                ../engine/source/elements/sph/sppro3.F
      !||    spscomp               ../engine/source/elements/sph/spcompl.F
      !||    spsgsym               ../engine/source/elements/sph/spsgsym.F
      !||    spstabs               ../engine/source/elements/sph/spstab.F
      !||    spstabw               ../engine/source/elements/sph/spstab.F
      !||    spsym_alloc           ../engine/source/elements/sph/spsym_alloc.F
      !||    spsymp                ../engine/source/elements/sph/spsym.F
      !||    sptrivox              ../engine/source/elements/sph/sptrivox.F
      !||====================================================================
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
