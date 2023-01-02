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
Chd|  CHECK_NAN_ACC                 source/output/outfile/check_nan_acc.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PREPARE_DEBUG                 share/modules/debug_mod.F     
Chd|        SPMD_BARRIER                  source/mpi/generic/spmd_barrier.F
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|====================================================================
      SUBROUTINE CHECK_NAN_ACC(NCYCLE,A,AR)

      USE DEBUG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com04_c.inc"
#include "scr07_c.inc"
#include "task_c.inc"
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------
!       REAL ou REAL*8
        INTEGER, INTENT(IN) :: NCYCLE
        my_real, DIMENSION(3,NUMNOD), INTENT(IN) :: A,AR
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       NCYCLE : integer, current cycle
!       A : my_real ; dimension=NUMNOD
!           acceleration
!       AR : my_real ; dimension=NUMNOD
!           R acceleration
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: BOOL_NAN,MY_ISNAN
        INTEGER :: N
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
!$COMMENT
!       CHECK_NAN_ACC description
!       check if a NaN appears in A/AR arrays
!       
!       CHECK_NAN_ACC organization :
!       - at T=0, need to initialize the ITAB_DEBUG array in order to
!         print the global node ID
!       - check if a NaN appears in A/AR arrays 
!       - if a NaN is present, print the ID node, the processor and
!         the cycle and then stop the run with MSTOP=1
!$ENDCOMMENT

        IF(.NOT.ALLOCATED(ITAB_DEBUG)) CALL PREPARE_DEBUG()

        BOOL_NAN = .FALSE.
        DO N=1,NUMNOD
                IF(MY_ISNAN(A(1,N)).OR.MY_ISNAN(A(2,N)).OR.MY_ISNAN(A(3,N))) THEN
                        WRITE(IOUT,1000) ISPMD,NCYCLE
                        WRITE(IOUT,1001) N,ITAB_DEBUG(N)
                        WRITE(IOUT,1002) A(1,N),A(2,N),A(3,N)

                        WRITE(ISTDO,1000) ISPMD,NCYCLE
                        WRITE(ISTDO,1001) N,ITAB_DEBUG(N)
                        WRITE(ISTDO,1002) A(1,N),A(2,N),A(3,N)


                        BOOL_NAN = .TRUE.
                ENDIF

                IF(MY_ISNAN(AR(1,N)).OR.MY_ISNAN(AR(2,N)).OR.MY_ISNAN(AR(3,N))) THEN
                        WRITE(IOUT,1003) ISPMD,NCYCLE
                        WRITE(IOUT,1001) N,ITAB_DEBUG(N)
                        WRITE(IOUT,1004) AR(1,N),AR(2,N),AR(3,N)

                        WRITE(ISTDO,1003) ISPMD,NCYCLE
                        WRITE(ISTDO,1001) N,ITAB_DEBUG(N)
                        WRITE(ISTDO,1004) AR(1,N),AR(2,N),AR(3,N)

                        BOOL_NAN = .TRUE.
                ENDIF
        ENDDO

        CALL SPMD_BARRIER()

        IF(BOOL_NAN) THEN
                WRITE(IOUT,1005)
                WRITE(ISTDO,1005)
                MSTOP=1
!                CALL FLUSH(6)
!                CALL ARRET(2)
        ENDIF

 1000 FORMAT(3X,'NaN detected on A array on processor ID',I10,' at cycle',I10)
 1001 FORMAT(3X,'Local node ID: ',I10,' Global node ID:',I10 )
 1002 FORMAT(3X,'A values: ',1PE10.3,1PE10.3,1PE10.3)
 1003 FORMAT(3X,'NaN detected on AR array on processor ID',I10,' at cycle',I10)
 1004 FORMAT(3X,'AR values: ',1PE10.3,1PE10.3,1PE10.3)
 1005 FORMAT(/,3X,'********************* NaN ********************* ',/,
     .         3X,'******** I"m going to kill the run now ******** ',/,
     .         3X,'********************* NaN ********************* ',/)



        RETURN
        END SUBROUTINE CHECK_NAN_ACC

