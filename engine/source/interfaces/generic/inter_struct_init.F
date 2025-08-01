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
!||    inter_struct_init   ../engine/source/interfaces/generic/inter_struct_init.F
!||--- called by ------------------------------------------------------
!||    resol               ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod       ../common_source/modules/interfaces/intbufdef_mod.F90
!||    inter_sorting_mod   ../engine/share/modules/inter_sorting_mod.F
!||    inter_struct_mod    ../engine/share/modules/inter_struct_mod.F
!||====================================================================
        SUBROUTINE INTER_STRUCT_INIT(INTER_STRUCT,SORT_COMM)
!$COMMENT
!       INTER_STRUCT_INIT description
!       initialization of INTER_STRUCT structure
!       
!       INTER_STRUCT_INIT organization :
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD  
        USE INTER_STRUCT_MOD
        USE INTER_SORTING_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: KK,NIN
!   ----------------------------------------
        DO KK=1,NINTER
            INTER_STRUCT(KK)%SIZE_CAND_A = 0   
            INTER_STRUCT(KK)%I_SK_OLD = 0    
            INTER_STRUCT(KK)%INACTI = 0
            INTER_STRUCT(KK)%INACTII = 0
            INTER_STRUCT(KK)%CURV_MAX_MAX = ZERO
            INTER_STRUCT(KK)%NMN_G = 0                
        ENDDO
        DO KK=1,NB_INTER_7_INACTI
            NIN = LIST_INTER_7_INACTI(KK)
            ALLOCATE( INTER_STRUCT(NIN)%NSNFIOLD(NSPMD) )
            INTER_STRUCT(NIN)%NSNFIOLD(1:NSPMD) = 0
        ENDDO
        DO KK=1,NINTER
            SORT_COMM(KK)%PROC_NUMBER = 0
            SORT_COMM(KK)%SIZE_GLOBAL_COARSE_CELL = 0
            ALLOCATE( SORT_COMM(KK)%DATA_PROC(NSPMD) )
            ALLOCATE( SORT_COMM(KK)%REQ_SD2(NSPMD) )
            ALLOCATE( SORT_COMM(KK)%REQ_SD3(NSPMD) )
        ENDDO

        RETURN
        END SUBROUTINE INTER_STRUCT_INIT
