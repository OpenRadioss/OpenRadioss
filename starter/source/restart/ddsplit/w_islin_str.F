Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  W_ISLIN_STR                   source/restart/ddsplit/w_islin_str.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        FRETITL                       source/starter/freform.F      
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE W_ISLIN_STR(IGRSLIN ,LEN_IA ,PROC, NODLOCAL)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN_IA,PROC,NODLOCAL(*)
!
      TYPE (SURF_) , DIMENSION(NSLIN) :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISU,I,J,K,ERR,NSEG,ELTYP,ELEM,P,
     .        L_SLIN,ID,TYPE,LEVEL,NSEG_R2R_ALL,
     .        NSEG_R2R_SHARE,NOD,ITITLE(LTITR),NSEG_L(NSLIN)
      CHARACTER(LEN=nchartitle) :: TITR
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  IGRSLIN_L
C-----------------------------------------------
!=======================================================================
      DO ISU=1,NSLIN
        TITR    = IGRSLIN(ISU)%TITLE
        CALL FRETITL(TITR,ITITLE,LTITR)
        CALL WRITE_I_C(ITITLE,LTITR)
      ENDDO ! DO ISU=1,NSLIN
      LEN_IA = LEN_IA + NSLIN
      ERR = 0
      ALLOCATE (IGRSLIN_L(LENISLIN), STAT=ERR)
      IGRSLIN_L(1:LENISLIN) = 0  
!
! COUNT LOCAL SEGMENTS "NSEG_L"
!
      DO ISU=1,NSLIN
        NSEG = IGRSLIN(ISU)%NSEG
        NSEG_L(ISU) = 0
        DO J=1,NSEG
          P = IGRSLIN(ISU)%PROC(J)
          IF (P == PROC +1) THEN
            NSEG_L(ISU) = NSEG_L(ISU) + 1
          ENDIF ! IF (P == PROC +1)
        ENDDO ! DO J=1,NSEG
      ENDDO ! DO ISU=1,NSLIN
!
      L_SLIN = 0
!
      DO ISU=1,NSLIN
        ID          = IGRSLIN(ISU)%ID
        NSEG        = IGRSLIN(ISU)%NSEG
        TYPE        = IGRSLIN(ISU)%TYPE
        LEVEL       = IGRSLIN(ISU)%LEVEL
        NSEG_R2R_ALL   = IGRSLIN(ISU)%NSEG_R2R_ALL
        NSEG_R2R_SHARE = IGRSLIN(ISU)%NSEG_R2R_SHARE
!
! line storage
!
        IGRSLIN_L(L_SLIN+1) = ID
          L_SLIN = L_SLIN+1
        IGRSLIN_L(L_SLIN+1) = NSEG_L(ISU)
          L_SLIN = L_SLIN+1
        IGRSLIN_L(L_SLIN+1) = TYPE
          L_SLIN = L_SLIN+1
        IGRSLIN_L(L_SLIN+1) = LEVEL
          L_SLIN = L_SLIN+1
        IGRSLIN_L(L_SLIN+1) = NSEG_R2R_ALL
          L_SLIN = L_SLIN+1
        IGRSLIN_L(L_SLIN+1) = NSEG_R2R_SHARE
          L_SLIN = L_SLIN+1
!
! LINE ENTITIES (NODES, PROC, ELTYP, ELEM)
!
        DO J=1,NSEG
          P     = IGRSLIN(ISU)%PROC(J)
          ELTYP = IGRSLIN(ISU)%ELTYP(J)
          ELEM  = IGRSLIN(ISU)%ELEM(J)
          IF (P == PROC +1) THEN
            DO K=1,2
              NOD = IGRSLIN(ISU)%NODES(J,K)
              L_SLIN = L_SLIN+1
            ENDDO
            IGRSLIN_L(L_SLIN+1) = P
            L_SLIN = L_SLIN+1
            IGRSLIN_L(L_SLIN+1) = ELTYP
            L_SLIN = L_SLIN+1
            IGRSLIN_L(L_SLIN+1) = ELEM   
            L_SLIN = L_SLIN+1
          ENDIF ! IF (P == PROC +1)
        ENDDO ! DO J=1,NSEG
      ENDDO ! DO ISU=1,NSLIN
!---------
      CALL WRITE_I_C(IGRSLIN_L,L_SLIN)
!---------
      DEALLOCATE (IGRSLIN_L)
!---------
      LEN_IA = LEN_IA + L_SLIN
!---------
      RETURN
      END
