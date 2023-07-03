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
Chd|  INTER_CHECK_SORT              source/interfaces/generic/inter_check_sort.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
        SUBROUTINE INTER_CHECK_SORT( ITASK,NEED_TO_SORT,NBINTC,INTLIST,IPARI,NSENSOR,
     .                               INTBUF_TAB,SENSOR_TAB,NB_INTER_SORTED,
     .                               LIST_INTER_SORTED,INTER_STRUCT)
!$COMMENT
!       INTER_CHECK_SORT description
!       check if a interface must be sorted
!       
!       INTER_CHECK_SORT organization :
!       loop over the NBINTC interfaces and if
!           * dist < 0 & t_start < current time < t_stop --> must be sorted
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD  
      USE INTER_STRUCT_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK    !   omp task id
        INTEGER, INTENT(inout) :: NEED_TO_SORT  !   1 if one or more interfaces must be sorted
        INTEGER, INTENT(in) :: NBINTC   ! number of interface which are not TYPE2
        INTEGER, INTENT(in) :: NSENSOR
        INTEGER, DIMENSION(NBINTC), INTENT(in) :: INTLIST   !   interface id 
        INTEGER, INTENT(inout) :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
        INTEGER, DIMENSION(NBINTC), INTENT(inout) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI    !   interface data
        TYPE(INTBUF_STRUCT_),DIMENSION(NINTER) :: INTBUF_TAB    ! interface data
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: NTY,INACTI,ISENS
        my_real :: TS,STARTT,STOPT,DIST
        LOGICAL TYPE18
        LOGICAL :: IS_SORTING_NEEDED
        INTEGER :: KK,N
!   ----------------------------------------
        NB_INTER_SORTED = 0
        ! --------------------------
        ! loop over the interface
        DO KK=1,NBINTC
            ! find the interface type
            N   = INTLIST(KK)
            NTY = IPARI(7,N)
            INACTI = IPARI(22,N)
            TYPE18=.FALSE.
            IF(NTY==7 .AND. INACTI==7)TYPE18=.TRUE.

            ISENS = 0
            IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.NTY == 25) ISENS = IPARI(64,N)  
            IF (ISENS > 0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
                TS = SENSOR_TAB(ISENS)%TSTART
            ELSE
                TS = TT
            ENDIF
            IS_SORTING_NEEDED=.FALSE.
            ! ------------------------
            ! interface type 7
            IF( (NTY == 7.AND.TT>=TS).AND.(.NOT.TYPE18) )THEN
                ! check if the interface must be sorted (dist + t_start + t_stop)
                DIST = INTBUF_TAB(N)%VARIABLES(5)
                IF (DIST<=ZERO) IS_SORTING_NEEDED = .TRUE.
                STARTT=INTBUF_TAB(N)%VARIABLES(3)
                STOPT =INTBUF_TAB(N)%VARIABLES(11)

                IF(IS_SORTING_NEEDED) THEN
                    IF(STARTT>TT) IS_SORTING_NEEDED = .FALSE.
                ENDIF

                IF(IS_SORTING_NEEDED) THEN
                    IF(TT>STOPT) IS_SORTING_NEEDED = .FALSE.
                ENDIF
                ! the current interface must be sorted --> IS_SORTING_NEEDED=true
                IF (IS_SORTING_NEEDED) THEN
                    IF(ITASK==0) NEED_TO_SORT = 1
                    NB_INTER_SORTED = NB_INTER_SORTED + 1
                    LIST_INTER_SORTED(NB_INTER_SORTED) = N
                    IF(ITASK==0) INTER_STRUCT(N)%CURV_MAX_MAX = ZERO
                ENDIF
            ENDIF
            ! ------------------------
        ENDDO
        ! --------------------------
        RETURN
        END SUBROUTINE INTER_CHECK_SORT
