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
Chd|  GRAVIT                        source/loads/general/grav/gravit.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        FINTER_SMOOTH                 source/tools/curve/finter_smooth.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE GRAVIT(IGRV  ,AGRV  ,NPC   ,TF   ,A     ,
     2                  V     ,X      ,SKEW  ,MS   ,SENSOR_TAB,
     3                  WEIGHT,IB    ,ITASK, NSENSOR, PYTHON)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
      USE PYTHON_FUNCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITASK
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NPC(*)
      INTEGER IGRV(NIGRV,*),IB(*)
      INTEGER WEIGHT(*)
      my_real
     .   AGRV(LFACGRV,*), TF(*), A(3,*), V(3,*), MS(*),
     .   X(3,*), SKEW(LSKEW,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      TYPE (PYTHON_), INTENT(IN) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, IFUNC, K1, K2, K3, ISENS,K,NN,
     .    IAD,J, IADF, IADL, ISMOOTH
C     REAL
      my_real
     .   AXI, A0, AA, VV, DYDX, TS,
     .   GAMA, TFEXTT,FCX,FCY
      my_real
     .   FINTER,FINTER_SMOOTH
      EXTERNAL FINTER,FINTER_SMOOTH
C=======================================================================
      TFEXTT=ZERO
C
      DO NL=1,NGRAV
        FCY = AGRV(1,NL)
        FCX = AGRV(2,NL)
        NN=IGRV(1,NL)
        ISK=IGRV(2,NL)/10
        N2 =IGRV(2,NL)-10*ISK
        IFUNC=IGRV(3,NL)
        IAD=IGRV(4,NL)
        ISMOOTH = 0
        IF (IFUNC > 0) ISMOOTH = NPC(2*NFUNCT+IFUNC+1)
        IADF = IAD+ITASK*NN/NTHREAD
        IADL = IAD-1+(ITASK+1)*NN/NTHREAD
C
        ISENS=0
        DO K=1,NSENSOR
          IF(IGRV(6,NL)== SENSOR_TAB(K)%SENS_ID) ISENS=K
        ENDDO
        IF(ISENS==0)THEN
          TS=TT
        ELSE
          TS = TT- SENSOR_TAB(ISENS)%TSTART
          IF(TS<0.0)CYCLE
        ENDIF
C
        IF (IFUNC > 0) THEN
!!          A0   = FCY*FINTER(IFUNC,(TS-DT1)*FCX,NPC,TF,DYDX)
!!          GAMA = FCY*FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
          IF (ISMOOTH == 0) THEN
            A0   = FCY*FINTER(IFUNC,(TS-DT1)*FCX,NPC,TF,DYDX)
            GAMA = FCY*FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
          ELSE IF (ISMOOTH > 0) THEN
            A0   = FCY*FINTER_SMOOTH(IFUNC,(TS-DT1)*FCX,NPC,TF,DYDX)
            GAMA = FCY*FINTER_SMOOTH(IFUNC,TS*FCX,NPC,TF,DYDX)
          ELSE 
            ISMOOTH = -ISMOOTH ! the id the python function is saved in the position of ISMOOTH in the NPC array 
            CALL PYTHON_CALL_FUNCT1D(PYTHON, ISMOOTH,(TS-DT1)*FCX, A0)
            CALL PYTHON_CALL_FUNCT1D(PYTHON, ISMOOTH,TS*FCX, GAMA)
          ENDIF
        ELSE
          A0   = FCY
          GAMA = FCY
        ENDIF
C
        AA = GAMA
        IF(N2D==1.AND.ISK<=1)THEN
#include "vectorize.inc"
          DO J=IADF,IADL
            N1=IABS(IB(J))
            AXI=X(2,N1)
            A(N2,N1)=A(N2,N1)+AA
            IF(IB(J)>0)TFEXTT=TFEXTT+HALF*(A0+AA)*MS(N1)*V(N2,N1)*DT1*AXI*WEIGHT(N1)
          ENDDO
        ELSEIF(N2D==1.AND.ISK>1)THEN
          K1=3*N2-2
          K2=3*N2-1
          K3=3*N2
#include "vectorize.inc"
          DO J=IADF,IADL
            N1=IABS(IB(J))
            AXI=X(2,N1)
            VV = SKEW(K1,ISK)*V(1,N1)+SKEW(K2,ISK)*V(2,N1)+SKEW(K3,ISK)*V(3,N1)
            A(1,N1)=A(1,N1)+SKEW(K1,ISK)*AA
            A(2,N1)=A(2,N1)+SKEW(K2,ISK)*AA
            A(3,N1)=A(3,N1)+SKEW(K3,ISK)*AA
            IF(IB(J)>0)TFEXTT=TFEXTT+HALF*(A0+AA)*MS(N1)*VV*DT1*AXI*WEIGHT(N1)
          ENDDO
        ELSEIF(ISK<=1)THEN
#include "vectorize.inc"
          DO J=IADF,IADL
            N1=IABS(IB(J))
            A(N2,N1)=A(N2,N1)+AA
            IF(IB(J)>0)TFEXTT=TFEXTT+HALF*(A0+AA)*MS(N1)*V(N2,N1)*DT1*WEIGHT(N1)
          ENDDO
        ELSE
          K1=3*N2-2
          K2=3*N2-1
          K3=3*N2
#include "vectorize.inc"
          DO J=IADF,IADL
            N1=IABS(IB(J))
            VV = SKEW(K1,ISK)*V(1,N1)+SKEW(K2,ISK)*V(2,N1)+ SKEW(K3,ISK)*V(3,N1)
            A(1,N1)=A(1,N1)+SKEW(K1,ISK)*AA
            A(2,N1)=A(2,N1)+SKEW(K2,ISK)*AA
            A(3,N1)=A(3,N1)+SKEW(K3,ISK)*AA
            IF(IB(J)>0)TFEXTT=TFEXTT+HALF*(A0+AA)*MS(N1)*VV*DT1*WEIGHT(N1)
          ENDDO
        ENDIF
C       /---------------/
        CALL MY_BARRIER
C       /---------------/
      END DO
C
#include "atomic.inc"
      TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
C
      RETURN
      END
