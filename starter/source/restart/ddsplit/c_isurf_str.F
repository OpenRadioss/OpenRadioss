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
Chd|  C_ISURF_STR                   source/restart/ddsplit/c_isurf_str.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
        SUBROUTINE C_ISURF_STR(PROC,LENISURF_L,NSPMD,IGRSURF_PROC)
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
        INTEGER, INTENT(IN) :: PROC !< processor id
        INTEGER, INTENT(INOUT) :: LENISURF_L !< size of surface buffer written in the restart
        INTEGER, INTENT(IN) :: NSPMD !<  number of processor
        TYPE(SURF_), DIMENSION(NSURF,NSPMD), INTENT(IN) :: IGRSURF_PROC !< surface structure per proc, size =NSURF*NSPMS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: ISU
        INTEGER :: L_SURF
C-----------------------------------------------
        L_SURF = 0
!
        DO ISU=1,NSURF
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
            L_SURF = 6*IGRSURF_PROC(ISU,PROC+1)%NSEG + L_SURF
        ENDDO
!---------
        LENISURF_L = L_SURF
!---------
        RETURN
        END SUBROUTINE C_ISURF_STR
