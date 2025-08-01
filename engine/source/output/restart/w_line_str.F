Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    w_line_str             ../engine/source/output/restart/w_line_str.F
!||--- called by ------------------------------------------------------
!||    wrrestp                ../engine/source/output/restart/wrrestp.F
!||--- calls      -----------------------------------------------------
!||    fretitl                ../engine/source/input/freform.F
!||    write_i_c              ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    groupdef_mod           ../common_source/modules/groupdef_mod.F
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE W_LINE_STR(IGRSLIN)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE GROUPDEF_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARTITLE
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
      TYPE (SURF_) , DIMENSION(NSLIN) :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISU,I,J,K,ERR,L_SLIN,ID,NSEG,TYPE,PROC,
     .        LEVEL,NSEG_R2R_ALL,NSEG_R2R_SHARE,
     .        NODE,ELTYP,ELEM,ITITLE(LTITR)
      CHARACTER(LEN=NCHARTITLE) :: TITR
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  ISLINI
C-----------------------------------------------
      DO ISU=1,NSLIN
        TITR    = IGRSLIN(ISU)%TITLE
        CALL FRETITL(TITR,ITITLE,LTITR)
        CALL WRITE_I_C(ITITLE,LTITR)
      ENDDO ! DO ISU=1,NSLIN
!
      ERR = 0
      ALLOCATE (ISLINI(LENISLIN), STAT=ERR)
      ISLINI(1:LENISLIN) = 0  
      L_SLIN = 0
!
      DO ISU=1,NSLIN
        ID             = IGRSLIN(ISU)%ID
        NSEG           = IGRSLIN(ISU)%NSEG
        TYPE           = IGRSLIN(ISU)%TYPE
        LEVEL          = IGRSLIN(ISU)%LEVEL
        NSEG_R2R_ALL   = IGRSLIN(ISU)%NSEG_R2R_ALL
        NSEG_R2R_SHARE = IGRSLIN(ISU)%NSEG_R2R_SHARE
!
        ISLINI(L_SLIN+1) = ID
          L_SLIN = L_SLIN+1
        ISLINI(L_SLIN+1) = NSEG
          L_SLIN = L_SLIN+1
        ISLINI(L_SLIN+1) = TYPE
          L_SLIN = L_SLIN+1
        ISLINI(L_SLIN+1) = LEVEL
          L_SLIN = L_SLIN+1
        ISLINI(L_SLIN+1) = NSEG_R2R_ALL
          L_SLIN = L_SLIN+1
        ISLINI(L_SLIN+1) = NSEG_R2R_SHARE
          L_SLIN = L_SLIN+1
!
! LINE ENTITIES (NODES, PROC, ELTYP, ELEM)
!
        DO J=1,NSEG
          DO K=1,2
            NODE = IGRSLIN(ISU)%NODES(J,K)
            ISLINI(L_SLIN+1) = NODE
              L_SLIN = L_SLIN+1
          ENDDO
          PROC = IGRSLIN(ISU)%PROC(J)
          ISLINI(L_SLIN+1) = PROC
            L_SLIN = L_SLIN+1
          ELTYP = IGRSLIN(ISU)%ELTYP(J)
          ISLINI(L_SLIN+1) = ELTYP
            L_SLIN = L_SLIN+1
          ELEM = IGRSLIN(ISU)%ELEM(J)
          ISLINI(L_SLIN+1) = ELEM
            L_SLIN = L_SLIN+1
        ENDDO
      ENDDO ! DO ISU=1,NSLIN
!
!---------
      CALL WRITE_I_C(ISLINI,L_SLIN)
!---------
      DEALLOCATE (ISLINI)
!---------
      RETURN
      END
