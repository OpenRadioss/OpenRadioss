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
!||    rmatforp          ../engine/source/materials/mat/mat013/rmatforp.F
!||--- called by ------------------------------------------------------
!||    resol             ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    rmatpon           ../engine/source/materials/mat/mat013/rmatpon.F
!||    spmd_exch_a_rm6   ../engine/source/mpi/kinematic_conditions/spmd_exch_a_rm6.F
!||    startime          ../engine/source/system/timer_mod.F90
!||    stoptime          ../engine/source/system/timer_mod.F90
!||--- uses       -----------------------------------------------------
!||    timer_mod         ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE RMATFORP(TIMERS,
     1   A       ,AR       ,X        ,VR     ,IN     ,
     2   STIFN   ,STIFR    ,IRBY     ,LNRBY  ,RBY    , 
     3   ICODRBY ,WEIGHT   ,MS       ,V      ,FR_RBM ,
     4   IAD_RBY ,ARBY    ,VRBY     ,ARRBY   ,VRRBY  ,
     5   KIND    ,RBYM6   )
        USE TIMER_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER FR_RBM(*),IAD_RBY(*),
     .        ICODRBY(2,*),WEIGHT(*),LNRBY(*),IRBY(NIRBYM,*),
     .        KIND(NRBYM)
C     REAL
      my_real
     .   RBY(NFRBYM,*) ,A(3,*) ,AR(3,*) ,X(3,*) ,VR(3,*),
     .   IN(*) ,STIFN(*),STIFR(*),MS(*),V(3,*) , 
     .   VRRBY(3,*),ARBY(3,*),ARRBY(3,*),
     .   VRBY(3,*)
      DOUBLE PRECISION RBYM6(6,6,NRBYM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  K,M

C-------------------------------------
C Traitement rigid material  
C-------------------------------------

!$OMP SINGLE

      K=1
      DO M=1,NRBYM
        KIND(M) = K
        K  = K  + IRBY(2,M)
      ENDDO

!$OMP END SINGLE

!$OMP DO SCHEDULE(DYNAMIC,1)
        DO M=1,NRBYM
          K = KIND(M)
          CALL RMATPON(
     1      A           ,AR       ,X      ,RBY(1,M),LNRBY(K) ,
     2      IRBY(1,M)   ,STIFN    ,STIFR  ,WEIGHT  ,IRBY(2,M),
     3      RBYM6(1,1,M),ICODRBY  ,ARBY   ,VRBY    ,ARRBY    ,
     3      VRRBY    ,1        )
        ENDDO
!$OMP END DO

        IF (NSPMD > 1) THEN
!$OMP SINGLE
          IF (IMON>0) CALL STARTIME(TIMERS,11)
C
C Comm non multi-thread
C
          CALL SPMD_EXCH_A_RM6(
     1         IAD_RBY,FR_RBM,IAD_RBY(NSPMD+1),RBYM6 )
          IF (IMON>0) CALL STOPTIME(TIMERS,11)
!$OMP END SINGLE

        END IF
C
C Traitement fin parith/on multi-thread
C

!$OMP DO SCHEDULE(DYNAMIC,1)
        DO M=1,NRBYM
          K = KIND(M)
          CALL RMATPON(
     1      A           ,AR      ,X     ,RBY(1,M),LNRBY(K) ,
     2      IRBY(1,M)   ,STIFN   ,STIFR ,WEIGHT  ,IRBY(2,M),
     4      RBYM6(1,1,M),ICODRBY ,ARBY  ,VRBY    ,ARRBY    ,
     3      VRRBY       ,2       )
        ENDDO
!$OMP END DO
C
      RETURN
      END
