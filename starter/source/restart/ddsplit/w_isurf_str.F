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
Chd|  W_ISURF_STR                   source/restart/ddsplit/w_isurf_str.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        FRETITL                       source/starter/freform.F      
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
        SUBROUTINE W_ISURF_STR( LEN_IA,PROC,NUMNOD,NSURF,NUMELS,
     .                          NUMELQ,NUMELC,NUMELT,NUMELP,NUMELR,
     .                          NODLOCAL,SCEL,CEL,LTITR,LENISURF_L,
     .                          NSPMD,IGRSURF,IGRSURF_PROC)
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
#include "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(INOUT) :: LEN_IA!< size of the restart
        INTEGER, INTENT(IN) :: PROC !< processor id
        INTEGER, INTENT(IN) :: NUMNOD !< number of node
        INTEGER, INTENT(IN) :: NSURF  !< number of surface
        INTEGER, INTENT(IN) :: NUMELS !< number of solid
        INTEGER, INTENT(IN) :: NUMELQ !< number of quad
        INTEGER, INTENT(IN) :: NUMELC !< number of shell
        INTEGER, INTENT(IN) :: NUMELT !< number of truss
        INTEGER, INTENT(IN) :: NUMELP !< number of beam
        INTEGER, INTENT(IN) :: NUMELR !< number of spring
        INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: NODLOCAL !< array to convert global node id to local node id
        INTEGER, INTENT(IN) :: SCEL !< size of CEL
        INTEGER, DIMENSION(SCEL), INTENT(IN) :: CEL !< connectivity global element id --> local element id
        INTEGER, INTENT(IN) :: LENISURF_L !< size of surface buffer written in the restart
        INTEGER, INTENT(IN) :: LTITR !< size of ititle integer array
        INTEGER, INTENT(IN) :: NSPMD !<  number of processor
        TYPE (SURF_) , DIMENSION(NSURF) :: IGRSURF !< surface structure, size =NSURF
        TYPE(SURF_), DIMENSION(NSURF,NSPMD), INTENT(IN) :: IGRSURF_PROC !< surface structure per proc, size =NSURF*NSPMS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: ISU,J,K,ERR,NSEG
        INTEGER :: L_SURF,ID,TYPE,ID_MADYMO,IAD_BUFR
        INTEGER :: NB_MADYMO,TYPE_MADYMO,LEVEL,TH_SURF,ISH4N3N,NSEG_R2R_ALL
        INTEGER :: NSEG_R2R_SHARE
        INTEGER, DIMENSION(LTITR) :: ITITLE
        INTEGER, DIMENSION(NSURF) :: NSEG_L
        CHARACTER(LEN=nchartitle) :: TITR
        INTEGER, ALLOCATABLE, DIMENSION (:)  ::  IGRSURF_L

        INTEGER :: JJ
        INTEGER :: NODE_ID,LOCAL_NODE_ID,ELEM,ELTYP
        INTEGER, DIMENSION(0:7) :: OFFSET ! offset array
C-----------------------------------------------
!=======================================================================
        OFFSET(0:7) = 0
        OFFSET(1) = 0 ! offset for solid
        OFFSET(2) = NUMELS ! offset for quad
        OFFSET(3) = NUMELS+NUMELQ ! offset for shell   
        OFFSET(7) = NUMELS+NUMELQ+ NUMELC+NUMELT+NUMELP+NUMELR ! offset for triangle

        DO ISU=1,NSURF
            TITR    = IGRSURF(ISU)%TITLE
            CALL FRETITL(TITR,ITITLE,LTITR)
            CALL WRITE_I_C(ITITLE,LTITR)
        ENDDO ! DO ISU=1,NSURF
        LEN_IA = LEN_IA + NSURF
!
        ERR = 0
        ALLOCATE (IGRSURF_L(LENISURF_L), STAT=ERR)
!
! COUNT LOCAL SEGMENTS "NSEG_L"
!
        DO ISU=1,NSURF
            NSEG      = IGRSURF(ISU)%NSEG
            NSEG_L(ISU) = IGRSURF_PROC(ISU,PROC+1)%NSEG
        ENDDO
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
            IGRSURF_L(L_SURF+1) = ID
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = NSEG_L(ISU)
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = TYPE
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = ID_MADYMO
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = IAD_BUFR
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = NB_MADYMO
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = TYPE_MADYMO
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = LEVEL
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = TH_SURF
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = ISH4N3N
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = NSEG_R2R_ALL
            L_SURF = L_SURF+1
            IGRSURF_L(L_SURF+1) = NSEG_R2R_SHARE
            L_SURF = L_SURF+1
            DO JJ=1,IGRSURF_PROC(ISU,PROC+1)%NSEG
                J = IGRSURF_PROC(ISU,PROC+1)%LOCAL_SEG(JJ)
                DO K=1,4
                    NODE_ID = IGRSURF(ISU)%NODES(J,K)
                    IF(NODE_ID/=0) THEN
                        LOCAL_NODE_ID = NODLOCAL(NODE_ID)
                    ELSE
                        LOCAL_NODE_ID = 0
                    ENDIF
                    IGRSURF_L(L_SURF+1) = LOCAL_NODE_ID
                    L_SURF = L_SURF+1
                ENDDO
                ELTYP = IGRSURF_PROC(ISU,PROC+1)%ELTYP(JJ)
                ELEM = IGRSURF_PROC(ISU,PROC+1)%ELEM(JJ) + OFFSET(ELTYP)
                IF(ELEM/=0) ELEM = CEL(ELEM)           
                IGRSURF_L(L_SURF+1) = ELTYP
                L_SURF = L_SURF+1
                IGRSURF_L(L_SURF+1) = ELEM
                L_SURF = L_SURF+1
            ENDDO
        ENDDO
!---------
      CALL WRITE_I_C(IGRSURF_L,L_SURF)
!---------
      DEALLOCATE (IGRSURF_L)
!---------
      LEN_IA = LEN_IA + L_SURF
!---------
      RETURN
      END SUBROUTINE W_ISURF_STR
