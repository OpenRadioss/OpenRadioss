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
Chd|====================================================================
Chd|  INIT_TH_GROUP                 source/output/th/init_th_group.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE INIT_TH_GROUP(GR       ,IGR     ,NELEM    ,NGRTH   ,IPARG  ,
     .                         IPART    ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N,
     .                         IGRTRUSS ,IGRBEAM ,IGRSPRING)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "vect04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGR(*),GR(*),NELEM,NGRTH,
     .        IPARG(NPARG,*),IPART(LIPART1,*)
C-----------------------------------------------
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRSPRI) :: IGRSPRING
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID,NN,NVAR,ITYP,CPT,GRTMP,IGRTMP(NGRTH+NELEM),
     .       IGR1,FLAG,OFFSET,NG,CPT1,IGRELE,NENTITY
!
      INTEGER, DIMENSION(:), POINTER :: ELEM
C-----------------------------------------------
      CPT = 1
      IGRTMP = 0
      OFFSET = 0
      CPT1 = 0
      DO K=NPART+1,NPART+NTHPART
        I = IPART(1,K)
        CPT1 = CPT1 + 1
!---
        OFFSET = 0
        IGRELE = IPART(1,K)
        ITYP   = IPART(2,K)
        ID     = IPART(4,K)
        NENTITY = 0
!
        IF (ITYP == 1) THEN  ! brick group of thpart
          NENTITY = IGRBRIC(IGRELE)%NENTITY
          ELEM   => IGRBRIC(IGRELE)%ENTITY
        ELSEIF (ITYP == 2) THEN  ! quad group of thpart
          OFFSET = OFFSET + NUMELS
          NENTITY = IGRQUAD(IGRELE)%NENTITY
          ELEM   => IGRQUAD(IGRELE)%ENTITY
        ELSEIF (ITYP == 3) THEN  ! sh4n group of thpart
          OFFSET = OFFSET + NUMELQ
          NENTITY = IGRSH4N(IGRELE)%NENTITY
          ELEM   => IGRSH4N(IGRELE)%ENTITY
        ELSEIF (ITYP == 4) THEN  ! truss group of thpart
          OFFSET = OFFSET + NUMELC
          NENTITY = IGRTRUSS(IGRELE)%NENTITY
          ELEM   => IGRTRUSS(IGRELE)%ENTITY
        ELSEIF (ITYP == 5) THEN  ! beam group of thpart
          OFFSET = OFFSET + NUMELT
          NENTITY = IGRBEAM(IGRELE)%NENTITY
          ELEM   => IGRBEAM(IGRELE)%ENTITY
        ELSEIF (ITYP == 6) THEN  ! spring group of thpart
          OFFSET = OFFSET + NUMELP
          NENTITY = IGRSPRING(IGRELE)%NENTITY
          ELEM   => IGRSPRING(IGRELE)%ENTITY
        ELSEIF (ITYP == 7) THEN  ! SH3N group of thpart
          OFFSET = OFFSET + NUMELR
          NENTITY = IGRSH3N(IGRELE)%NENTITY
          ELEM   => IGRSH3N(IGRELE)%ENTITY
        ENDIF ! IF (ITYP == 1)
!---
        DO J=1,NENTITY
          IGRTMP(CPT) = ELEM(J)+OFFSET
          GR(CPT) = CPT1
          IGR(ELEM(J)+OFFSET) = IGR(ELEM(J)+OFFSET) + 1
          CPT = CPT + 1
        ENDDO ! DO J=1,NENTITY
      ENDDO ! DO K=NPART+1,NPART+NTHPART
!
      DO I=1,CPT-1
        DO J=I,CPT-1
          IF (IGRTMP(I) > IGRTMP(J)) THEN
            GRTMP = GR(J)
            GR(J) = GR(I)
            GR(I) = GRTMP
            GRTMP = IGRTMP(J)
            IGRTMP(J)= IGRTMP(I)
            IGRTMP(I) = GRTMP
          ENDIF
        ENDDO
      ENDDO
!
      IGRTMP = 0
      DO I = 1,NELEM
        IGRTMP(I) = IGR(I)
      ENDDO
!
      FLAG = 0
      IF (IGR(1) == 0) THEN
        IGR(1) = 1
      ELSE
        IGR(2) = 1 + IGR(1)
        IGR(1) = 1
        FLAG = 1
      ENDIF
!
      DO I = 2,NELEM
        IF (IGR(I) == 0) THEN
          IF (FLAG == 0) THEN
            IGR(I) = IGR(I-1)
          ELSEIF (FLAG == 1) THEN
            IGR(I) = IGR(I)
            FLAG = 0
          ENDIF
        ELSE
          IF (FLAG == 0) THEN
            IGR(I) =  IGR(I-1)
            IGR(I+1) = IGR(I) + IGRTMP(I)
          ELSEIF (FLAG == 1) THEN
            IGR(I) =  IGR(I)
            IGR(I+1) = IGR(I) + IGRTMP(I)
          ENDIF
          FLAG = 1
        ENDIF
      ENDDO
!
      DO NG=1,NGROUP
        NEL     = IPARG(2,NG)
        NFT     = IPARG(3,NG)
        ITYP    = IPARG(5,NG)
        IF (ITYP == 1) OFFSET = 0
        IF (ITYP == 2) OFFSET = NUMELS
        IF (ITYP == 3) OFFSET = NUMELS + NUMELQ
        IF (ITYP == 4) OFFSET = NUMELS + NUMELQ + NUMELC
        IF (ITYP == 5) OFFSET = NUMELS + NUMELQ + NUMELC
     .                           + NUMELT
        IF (ITYP == 6) OFFSET = NUMELS + NUMELQ + NUMELC
     .                           + NUMELT + NUMELP
        IF (ITYP == 7) OFFSET = NUMELS + NUMELQ + NUMELC
     .                           + NUMELT + NUMELP + NUMELR
        DO J=NFT+OFFSET+1,NFT+OFFSET+NEL
          IF (IGR(J) /= IGR(J+1)) THEN
            IPARG(51,NG) = 1
          ENDIF
        ENDDO
      ENDDO
!---
      RETURN
      END
