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
Chd|  CHECK_SURFACE_STATE           source/interfaces/interf/check_surface_state.F
Chd|-- called by -----------
Chd|        FIND_SURFACE_FROM_REMOTE_PROC source/interfaces/interf/find_surface_from_remote_proc.F
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
        SUBROUTINE CHECK_SURFACE_STATE( ITASK,SURFARCE_NB,SURFACE_ID,SHIFT_INTERFACE,INTBUF_TAB)
!$COMMENT
!       CHECK_SURFACE_STATE description
!           deactivation of surface from an interface
!       CHECK_SURFACE_STATE organization
!$ENDCOMMENT
        USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK ! omp thread ID
        INTEGER, INTENT(in) :: SURFARCE_NB  ! number of local deactivated surface
        INTEGER, DIMENSION(SURFARCE_NB), INTENT(in) :: SURFACE_ID  ! id of surface that need to be deactivated
        INTEGER, DIMENSION(NINTER+1,2), INTENT(in) :: SHIFT_INTERFACE ! interface shift
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,K,FIRST,LAST
        INTEGER :: NIN,ID_INTER,NUMBER_INTER
        INTEGER :: SHIFT
        INTEGER :: DICHOTOMIC_SEARCH_I_ASC  ! function
C-----------------------------------------------
        IF(ITASK==-1) THEN
            FIRST = 1
            LAST = SURFARCE_NB
        ELSE
            FIRST = 1 + ITASK * (SURFARCE_NB / NTHREAD)
            LAST = (ITASK + 1) * (SURFARCE_NB / NTHREAD)
            IF((ITASK+1)==NTHREAD) LAST = SURFARCE_NB
        ENDIF
        NUMBER_INTER = SHIFT_INTERFACE(NINTER+1,2)
        ! --------------------------
        ! loop over the deactivated surface
        DO I=FIRST,LAST
            K = SURFACE_ID(I)  ! get the global surface id
            ID_INTER = DICHOTOMIC_SEARCH_I_ASC(K, SHIFT_INTERFACE(1,1), NUMBER_INTER+1) ! find the interface of the surface
            NIN = SHIFT_INTERFACE(ID_INTER,2)
            K = K - SHIFT_INTERFACE(ID_INTER,1) + 1 ! get the surface id in the NIN interface
            ! check if the surface is active, if yes --> deactivate it
            IF(INTBUF_TAB(NIN)%STFM(K)/=ZERO) THEN
                INTBUF_TAB(NIN)%STFM(K) = ZERO
            ENDIF
        ENDDO
        ! --------------------------

        RETURN
        END SUBROUTINE CHECK_SURFACE_STATE
