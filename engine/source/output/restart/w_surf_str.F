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
Chd|====================================================================
Chd|  W_SURF_STR                    source/output/restart/w_surf_str.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        FRETITL                       source/input/freform.F        
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE W_SURF_STR(IGRSURF)
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
#include      "nchar_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SURF_) , DIMENSION(NSURF) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISU,I,J,K,ERR,L_SURF,ID,NSEG,TYPE,ID_MADYMO,IAD_BUFR,
     .        NB_MADYMO,TYPE_MADYMO,LEVEL,TH_SURF,ISH4N3N,NSEG_R2R_ALL,
     .        NSEG_R2R_SHARE,NODE,ELTYP,ELEM,ITITLE(LTITR)
      CHARACTER(LEN=nchartitle) :: TITR
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  ISURFI
C-----------------------------------------------
      DO ISU=1,NSURF
        TITR    = IGRSURF(ISU)%TITLE
        CALL FRETITL(TITR,ITITLE,LTITR)
        CALL WRITE_I_C(ITITLE,LTITR)
      ENDDO ! DO ISU=1,NSURF
!
      ERR = 0
      ALLOCATE (ISURFI(LENISURF), STAT=ERR)
      ISURFI(1:LENISURF) = 0  
      L_SURF = 0
!
      DO ISU=1,NSURF
        ID             = IGRSURF(ISU)%ID
        NSEG           = IGRSURF(ISU)%NSEG
        TYPE           = IGRSURF(ISU)%TYPE
        ID_MADYMO      = IGRSURF(ISU)%ID_MADYMO
        IAD_BUFR       = IGRSURF(ISU)%IAD_BUFR
        NB_MADYMO      = IGRSURF(ISU)%NB_MADYMO
        TYPE_MADYMO    = IGRSURF(ISU)%TYPE_MADYMO
        LEVEL          = IGRSURF(ISU)%LEVEL
        TH_SURF        = IGRSURF(ISU)%TH_SURF
        ISH4N3N        = IGRSURF(ISU)%ISH4N3N
        NSEG_R2R_ALL   = IGRSURF(ISU)%NSEG_R2R_ALL
        NSEG_R2R_SHARE = IGRSURF(ISU)%NSEG_R2R_SHARE
!
        ISURFI(L_SURF+1) = ID
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = NSEG
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = TYPE
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = ID_MADYMO
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = IAD_BUFR
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = NB_MADYMO
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = TYPE_MADYMO
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = LEVEL
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = TH_SURF
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = ISH4N3N
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = NSEG_R2R_ALL
          L_SURF = L_SURF+1
        ISURFI(L_SURF+1) = NSEG_R2R_SHARE
          L_SURF = L_SURF+1
!
! SURF ENTITIES (NODES, ELTYP, ELEM)
!
        DO J=1,NSEG
          DO K=1,4
            NODE = IGRSURF(ISU)%NODES(J,K)
            ISURFI(L_SURF+1) = NODE
              L_SURF = L_SURF+1
          ENDDO
          ELTYP = IGRSURF(ISU)%ELTYP(J)
          ISURFI(L_SURF+1) = ELTYP
            L_SURF = L_SURF+1
          ELEM = IGRSURF(ISU)%ELEM(J)
          ISURFI(L_SURF+1) = ELEM
            L_SURF = L_SURF+1
        ENDDO
      ENDDO ! DO ISU=1,NSURF
!
!---------
      CALL WRITE_I_C(ISURFI,L_SURF)
!---------
      DEALLOCATE (ISURFI)
!---------
      RETURN
      END
