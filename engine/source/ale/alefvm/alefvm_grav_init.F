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
Chd|  ALEFVM_GRAV_INIT              source/ale/alefvm/alefvm_grav_init.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_GRAV_INIT(
     1                          AGRV, IGRV, LGRAV, SENSOR_TAB,
     2                          IXS , ITASK, NPC , TF    ,SKEW    ,
     3                          ITAB )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandonned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C This subroutine is treating an uncut cell.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "nsvis_c.inc"
#include      "fluid_scr_c.inc"
#include      "inter22.inc"
#include      "param_c.inc"
#include      "alefvm.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C This subroutines computes gravity forces for
C finite volume scheme (IALEFVM==1)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXS(NIXS,*), IGRV(NIGRV,*), ITASK,LGRAV(*),NPC(*),ITAB(*)
      my_real :: AGRV(LFACGRV,*),TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER          :: I, J, K, K1, K2, K3, NL, NN, ISK, IFUNC, N2
      INTEGER          :: IADF,IADL,ISENS,N1,NVERTEX,INOd,II
      my_real          :: TS,AA,FCX,FCY,VV,A0,DYDX,GAMA,TFEXTT
      my_real          :: M_CELL(MVSIZ),ACCEL(3,MVSIZ),SKEW(LSKEW,*)
      my_real,EXTERNAL :: FINTER      
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
      TFEXTT = ZERO
  
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
          IF(IGRV(6,NL)==SENSOR_TAB(K)%SENS_ID) ISENS=K  ! do it in starter !!!
        ENDDO
        IF(ISENS==0)THEN
          TS  = TT
        ELSE
          TS  = TT-SENSOR_TAB(ISENS)%TSTART
          IF(TS<0.0)CYCLE
        ENDIF

        IF (IFUNC > 0) THEN
          A0    = FCY*FINTER(IFUNC,(TS-DT1)*FCX,NPC,TF,DYDX)
          GAMA  = FCY*FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
        ELSE
          A0   = FCY
          GAMA = FCY
        ENDIF
        AA    = GAMA

        K1    = 3*N2-2
        K2    = 3*N2-1
        K3    = 3*N2
        
        IF(NL==1)THEN
         !initialisation
#include "vectorize.inc"
          DO J=IADF,IADL
            !Acceleration on FVM vertexes
            N1=IABS(LGRAV(J))
            VERTEX(1,N1) =  SKEW(K1,ISK)*AA
            VERTEX(2,N1) =  SKEW(K2,ISK)*AA
            VERTEX(3,N1) =  SKEW(K3,ISK)*AA
            VERTEX(4,N1) = ONE
            !--- TODO : TFEXTT            
            ! VV = SKEW(K1,ISK)*V(1,N1)+SKEW(K2,ISK)*V(2,N1)+SKEW(K3,ISK)*V(3,N1)
            ! IF(LGRAV(J)>0)TFEXTT=TFEXTT+HALF*(A0+AA)*MS(N1)*VV*DT1*WEIGHT(N1)
           ENDDO !next J
         ELSE
          !cumul
#include "vectorize.inc"
          DO J=IADF,IADL
            !Acceleration on FVM vertexes
            N1=IABS(LGRAV(J))
            VERTEX(1,N1) = VERTEX(1,N1)+SKEW(K1,ISK)*AA
            VERTEX(2,N1) = VERTEX(2,N1)+SKEW(K2,ISK)*AA
            VERTEX(3,N1) = VERTEX(3,N1)+SKEW(K3,ISK)*AA
            VERTEX(4,N1) = ONE
            !--- TODO : TFEXTT            
            ! VV = SKEW(K1,ISK)*V(1,N1)+SKEW(K2,ISK)*V(2,N1)+SKEW(K3,ISK)*V(3,N1)
            ! IF(LGRAV(J)>0)TFEXTT=TFEXTT+HALF*(A0+AA)*MS(N1)*VV*DT1*WEIGHT(N1)
           ENDDO !next J
         ENDIF

      END DO !next NL



      RETURN
      END
