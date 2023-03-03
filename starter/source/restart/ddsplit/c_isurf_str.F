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
Chd|  C_ISURF_STR                   source/restart/ddsplit/c_isurf_str.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE C_ISURF_STR(IGRSURF ,LEN_IA ,CEP  ,CEL  ,PROC,
     .                       NODLOCAL,LENISURF_L)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN_IA,CEP(*),CEL(*),PROC,NODLOCAL(*),LENISURF_L
!
      TYPE (SURF_) , DIMENSION(NSURF) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISU,I,J,K,ERR,NSEG,ELTYP,ELEM,ESHIFT,
     .        L_SURF,ID,TYPE,ID_MADYMO,IAD_BUFR,
     .        NB_MADYMO,TYPE_MADYMO,LEVEL,TH_SURF,ISH4N3N,NSEG_R2R_ALL,
     .        NSEG_R2R_SHARE,NOD,ITITLE(LTITR),NSEG_L(NSURF)
      CHARACTER(LEN=nchartitle) ::  TITR
C-----------------------------------------------
!
! COUNT LOCAL SEGMENTS "NSEG_L"
!
      L_SURF = 0
!
      DO ISU=1,NSURF
        ID          = IGRSURF(ISU)%ID
        NSEG        = IGRSURF(ISU)%NSEG
        TYPE        = IGRSURF(ISU)%TYPE
        ID_MADYMO   = IGRSURF(ISU)%ID_MADYMO
        IAD_BUFR    = IGRSURF(ISU)%IAD_BUFR
        NB_MADYMO   = IGRSURF(ISU)%NB_MADYMO
        TYPE_MADYMO = IGRSURF(ISU)%TYPE_MADYMO
        LEVEL       = IGRSURF(ISU)%LEVEL
        TH_SURF     = IGRSURF(ISU)%TH_SURF
        ISH4N3N     = IGRSURF(ISU)%ISH4N3N
        NSEG_R2R_ALL   = IGRSURF(ISU)%NSEG_R2R_ALL
        NSEG_R2R_SHARE = IGRSURF(ISU)%NSEG_R2R_SHARE
!
! surf storage
!
!        IGRSURF_L(L_SURF+1) = ID
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = NSEG_L(ISU)
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = TYPE
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = ID_MADYMO
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = IAD_BUFR
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = NB_MADYMO
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = TYPE_MADYMO
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = LEVEL
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = TH_SURF
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = ISH4N3N
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = NSEG_R2R_ALL
          L_SURF = L_SURF+1
!        IGRSURF_L(L_SURF+1) = NSEG_R2R_SHARE
          L_SURF = L_SURF+1
!
! SURF ENTITIES (NODES, ELTYP, ELEM)
!
        DO J=1,NSEG
          ELTYP = IGRSURF(ISU)%ELTYP(J)
          ELEM  = IGRSURF(ISU)%ELEM(J)
          IF (ELEM > 0) THEN
            IF (ELTYP == 1) THEN
              ESHIFT = 0
            ELSEIF (ELTYP == 2) THEN
              ESHIFT = NUMELS
            ELSEIF (ELTYP == 3) THEN
              ESHIFT = NUMELS+NUMELQ
            ELSEIF (ELTYP == 7) THEN
              ESHIFT = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR
            ENDIF
            ELEM = ELEM + ESHIFT
            IF (CEP(ELEM) == PROC) THEN
              DO K=1,4
!                NOD = IGRSURF(ISU)%NODES(J,K)
!                IF(NOD > 0)THEN;IGRSURF_L(L_SURF+1) = NODLOCAL(NOD)
!	        ELSE;           IGRSURF_L(L_SURF+1) = 0
!	        ENDIF
                L_SURF = L_SURF+1
              ENDDO
!              IGRSURF_L(L_SURF+1) = ELTYP
              L_SURF = L_SURF+1
!              IGRSURF_L(L_SURF+1) = CEL(ELEM)
              L_SURF = L_SURF+1
            ENDIF ! IF (CEP(ELEM) == PROC)
          ELSE  !!! all the rest
            IF (PROC == 0) THEN
              DO K=1,4
!                NOD = IGRSURF(ISU)%NODES(J,K)
!                IF(NOD > 0)THEN;IGRSURF_L(L_SURF+1) = NODLOCAL(NOD)
!	        ELSE;           IGRSURF_L(L_SURF+1) = 0
!	        ENDIF
                L_SURF = L_SURF+1
              ENDDO
!              IGRSURF_L(L_SURF+1) = 0
              L_SURF = L_SURF+1
!              IGRSURF_L(L_SURF+1) = 0
              L_SURF = L_SURF+1
            ENDIF ! IF (PROC == 0)
          ENDIF ! IF (ELEM > 0) THEN
        ENDDO ! DO J=1,NSEG
      ENDDO ! DO ISU=1,NSURF
!---------
      LENISURF_L = L_SURF
!---------
      RETURN
      END
