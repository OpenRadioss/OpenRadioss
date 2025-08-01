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
!||    alefvm_grav_init      ../engine/source/ale/alefvm/alefvm_grav_init.F
!||--- called by ------------------------------------------------------
!||    alemain               ../engine/source/ale/alemain.F
!||--- calls      -----------------------------------------------------
!||    finter                ../engine/source/tools/curve/finter.F
!||--- uses       -----------------------------------------------------
!||    alefvm_mod            ../common_source/modules/ale/alefvm_mod.F
!||    python_funct_mod      ../common_source/modules/python_mod.F90
!||    sensor_mod            ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE ALEFVM_GRAV_INIT(PYTHON, 
     1                          AGRV, IGRV, LGRAV, NSENSOR, SENSOR_TAB,
     2                          ITASK, NPC , TF    ,SKEW )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandoned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C This subroutine is treating an uncut cell.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PYTHON_FUNCT_MOD
      USE ALEFVM_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C This subroutines computes gravity forces for
C finite volume scheme (IALEFVM==1)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PYTHON_) , INTENT(IN) :: PYTHON
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER :: IGRV(NIGRV,*), ITASK,LGRAV(*),NPC(*)
      my_real :: AGRV(LFACGRV,*),TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER          :: J, K, K1, K2, K3, NL, NN, ISK, IFUNC, N2
      INTEGER          :: IADF,IADL,ISENS,N1
      my_real          :: TS,AA,FCX,FCY,A0,DYDX,GAMA,WFEXTT
      my_real          :: SKEW(LSKEW,*)
      my_real,EXTERNAL :: FINTER      
      INTEGER          :: ISMOOTH
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
C      IF(IALEFVM==0)RETURN
      IF(NGRAV==0)  RETURN !no there is Call_my barrier
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      !-------------------------------------------------------------!
      ! GRAVITY COMPUTED FOR CELL VERTEXES                          !
      !-------------------------------------------------------------!   
      WFEXTT = ZERO
  
      DO NL=1,NGRAV
        FCY   = AGRV(1,NL)
        FCX   = AGRV(2,NL)
        NN    = IGRV(1,NL)
        ISK   = IGRV(2,NL)/10
        N2    = IGRV(2,NL)-10*ISK
        IFUNC = IGRV(3,NL)
        IAD   = IGRV(4,NL)
        IADF  = IAD+ITASK*NN/NTHREAD
        IADL  = IAD-1+(ITASK+1)*NN/NTHREAD

        ISENS=0
        DO K=1,NSENSOR
          IF(IGRV(6,NL)==SENSOR_TAB(K)%SENS_ID) ISENS=K  
        ENDDO
        IF(ISENS==0)THEN
          TS  = TT
        ELSE
          TS  = TT-SENSOR_TAB(ISENS)%TSTART
          IF(TS<0.0)CYCLE
        ENDIF
        ISMOOTH = 0
        IF (IFUNC > 0) ISMOOTH = NPC(2*NFUNCT+IFUNC+1)

        IF (IFUNC > 0) THEN
          IF(ISMOOTH >= 0) THEN
            A0    = FCY*FINTER(IFUNC,(TS-DT1)*FCX,NPC,TF,DYDX)
            GAMA  = FCY*FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
          ELSE IF (ISMOOTH < 0) THEN
            ISMOOTH = -ISMOOTH ! the id the python function is saved in the position of ISMOOTH in the NPC array 
            CALL PYTHON_CALL_FUNCT1D(PYTHON, ISMOOTH,(TS-DT1)*FCX, A0)
            CALL PYTHON_CALL_FUNCT1D(PYTHON, ISMOOTH,TS*FCX, GAMA)
            A0 = FCY*A0
            GAMA = FCY*GAMA
          ENDIF
        ELSE
          A0   = FCY
          GAMA = FCY
        ENDIF
        AA    = GAMA

        K1    = 3*N2-2
        K2    = 3*N2-1
        K3    = 3*N2
        
        IF(NL == 1)THEN
         !initialisation
#include "vectorize.inc"
          DO J=IADF,IADL
            !Acceleration on FVM vertexes
            N1=IABS(LGRAV(J))
            ALEFVM_Buffer%VERTEX(1,N1) =  SKEW(K1,ISK)*AA
            ALEFVM_Buffer%VERTEX(2,N1) =  SKEW(K2,ISK)*AA
            ALEFVM_Buffer%VERTEX(3,N1) =  SKEW(K3,ISK)*AA
            ALEFVM_Buffer%VERTEX(4,N1) = ONE
            !--- TODO : WFEXTT
            ! VV = SKEW(K1,ISK)*V(1,N1)+SKEW(K2,ISK)*V(2,N1)+SKEW(K3,ISK)*V(3,N1)
            ! IF(LGRAV(J)>0)WFEXTT=WFEXTT+HALF*(A0+AA)*MS(N1)*VV*DT1*WEIGHT(N1)
           ENDDO !next J
         ELSE
          !cumul
#include "vectorize.inc"
          DO J=IADF,IADL
            !Acceleration on FVM vertexes
            N1=IABS(LGRAV(J))
            ALEFVM_Buffer%VERTEX(1,N1) = ALEFVM_Buffer%VERTEX(1,N1)+SKEW(K1,ISK)*AA
            ALEFVM_Buffer%VERTEX(2,N1) = ALEFVM_Buffer%VERTEX(2,N1)+SKEW(K2,ISK)*AA
            ALEFVM_Buffer%VERTEX(3,N1) = ALEFVM_Buffer%VERTEX(3,N1)+SKEW(K3,ISK)*AA
            ALEFVM_Buffer%VERTEX(4,N1) = ONE
            !--- TODO : WFEXTT
            ! VV = SKEW(K1,ISK)*V(1,N1)+SKEW(K2,ISK)*V(2,N1)+SKEW(K3,ISK)*V(3,N1)
            ! IF(LGRAV(J)>0)WFEXTT=WFEXTT+HALF*(A0+AA)*MS(N1)*VV*DT1*WEIGHT(N1)
           ENDDO !next J
         ENDIF

      END DO !next NL



      RETURN
      END
