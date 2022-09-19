Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  RBYVIT                        source/constraints/general/rbody/rbyvit.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        RGBODV                        source/constraints/general/rbody/rgbodv.F
Chd|        SPMD_ALL_DMAX                 source/mpi/elements/spmd_sph.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RBYVIT(RBY   ,X       ,V        ,VR   ,SKEW,
     2                  FSAV  ,LPBY    ,NPBY     ,ISKEW,ITAB,
     3                  WEIGHT,A       ,AR       ,MS   ,IN  ,
     4                  KIND  ,IRBKIN_L,NRBYKIN_L,NODREAC,FTHREAC,
     5                  FREAC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "my_allocate.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr11_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ISKEW(*),ITAB(*),
     .        KIND(NRBYKIN),IRBKIN_L(*),NRBYKIN_L,NODREAC(*)
C     REAL
      my_real
     .   RBY(NRBY,*) ,X(3,*) ,V(3,*) ,VR(3,*),SKEW(*),
     .   FSAV(NTHVKI,*) ,A(3,*),AR(3,*),IN(*),MS(*),FTHREAC(*),FREAC(*)
C----------------------------------------------3
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  J,K,N,KK,IFAIL,ICOMM
      my_real
     .       FN, FT,EXPN,EXPT
      my_real, DIMENSION(:), ALLOCATABLE ::
     .       CRIT
C     REAL
      SAVE CRIT
C-----------------------------------------------
!$OMP SINGLE
       IF(NRBFAIL /= 0 .AND. NSPMD > 1)THEN
         MY_ALLOCATE(CRIT,NRBYKIN)
         CRIT(1:NRBYKIN) = ZERO
       ELSE
         ALLOCATE(CRIT(0))
       END IF
!$OMP END SINGLE
C-------------------------------------
C CALCUL SUPER RIGID BODIES (non multi-thread) sur premiere tache libre
C-------------------------------------

!$OMP SINGLE

        DO KK=1,NRBYKIN_L
          N=IRBKIN_L(KK)
          K  = KIND(N)
          IF(NPBY(7,N)>0.AND.NPBY(4,N)/=0)THEN
            J = NINTER+NRWALL+N
            IFAIL = NPBY(18,N)
            FN    = RBY(26,N)
            FT    = RBY(27,N)
            EXPN  = RBY(28,N)
            EXPT  = RBY(29,N)
C correction variables globales faites dans rbycor
            CALL RGBODV(V        ,VR         ,X     ,RBY(1,N)  ,LPBY(K),
     2                  NPBY(1,N),SKEW       ,ISKEW ,FSAV(1,J) ,ITAB   ,
     3                  WEIGHT   ,A         ,AR     ,MS        ,IN     ,
     4                  NPBY(4,N),NPBY(6,N) ,IFAIL  ,FN        ,EXPN   ,
     5                  FT       ,EXPT      ,RBY(30,N),NODREAC,FTHREAC ,
     6                  FREAC     )
C
            IF(NRBFAIL /= 0 .AND. NSPMD > 1) CRIT(N)= RBY(30,N)
C
          ENDIF
        ENDDO

!$OMP END SINGLE

C-------------------------------------
C CALCUL FORCE RIGID BODIES CLASSIQUES (multi-thread)
C-------------------------------------

!$OMP DO SCHEDULE(DYNAMIC,1)

        DO KK=1,NRBYKIN_L
          N=IRBKIN_L(KK)
          K  = KIND(N)
          IF(NPBY(7,N)>0.AND.NPBY(4,N)==0)THEN
            J = NINTER+NRWALL+N
            IFAIL = NPBY(18,N)
            FN    = RBY(26,N)
            FT    = RBY(27,N)
            EXPN  = RBY(28,N)
            EXPT  = RBY(29,N)
C correction variables globales faites dans rbycor
            CALL RGBODV(V        ,VR         ,X     ,RBY(1,N)  ,LPBY(K),
     2                  NPBY(1,N),SKEW       ,ISKEW ,FSAV(1,J) ,ITAB   ,
     3                  WEIGHT   ,A         ,AR     ,MS        ,IN     ,
     4                  NPBY(4,N),NPBY(6,N) ,IFAIL  ,FN        ,EXPN   ,
     5                  FT       ,EXPT      ,RBY(30,N),NODREAC,FTHREAC ,
     6                  FREAC     )
C
            IF(NRBFAIL /= 0 .AND. NSPMD > 1) CRIT(N)= RBY(30,N)
C
          ENDIF
        ENDDO

!$OMP END DO
C
!$OMP SINGLE
      IF(NRBFAIL /= 0 .AND. NSPMD > 1)THEN
        CALL SPMD_ALL_DMAX(CRIT,NRBYKIN)
        DO N=1,NRBYKIN
          RBY(30,N) = CRIT(N)
        ENDDO
      END IF
      DEALLOCATE(CRIT)
!$OMP END SINGLE
C
      RETURN
      END
