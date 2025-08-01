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
!||    cfield_1              ../engine/source/loads/general/load_centri/cfield.F
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    finter                ../engine/source/tools/curve/finter.F
!||    my_barrier            ../engine/source/system/machine.F
!||    relfram_m1            ../engine/source/tools/skew/relfram_m1.F
!||--- uses       -----------------------------------------------------
!||    python_funct_mod      ../common_source/modules/python_mod.F90
!||    sensor_mod            ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE CFIELD_1(PYTHON, ICFIELD  ,FAC   ,NPC   ,TF    ,A ,
     2                  V     ,X     ,XFRAME  ,MS,SENSOR_TAB,
     3                  WEIGHT,IB,ITASK,IFRAME,NSENSOR,WFEXT)
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
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PYTHON_), INTENT(INOUT) :: PYTHON
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NPC(*)
      INTEGER ICFIELD(SIZFIELD,*),IB(*),IFRAME(LISKN,*)
      INTEGER WEIGHT(*),ITASK
      my_real FAC(LFACLOAD,*), TF(*), A(3,*), V(3,*), MS(*),X(3,*), XFRAME(NXFRAME,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      DOUBLE PRECISION,INTENT(INOUT) :: WFEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, IFRA, N2, IFUNC, K1, K2, K3, ISENS,K,NN,IAD, J, PROC, IADF, IADL,IDIR,IFLAG,N1FRAM,IUN,JJ,IMOVFRAM
      INTEGER :: IS_TABULATED
      my_real NX, NY, NZ, AXI, A0, AA, VV, FX, FY, FZ, AX, DYDX, TS,
     .        GAMA, MA, VROT, X0, Y0, Z0, X1, Y1, Z1, X2, Y2, Z2, DWDT,
     .        WFEXTT,VMX,VMY,VMZ,VROT2
      my_real DIST(3),AREL(3),VN1FRAM(3),AN1FRAM(3),DW(3)
      my_real FINTER
C-----------------------------------------------
      EXTERNAL FINTER
      DATA IUN/1/
C-----------------------------------------------

      WFEXTT = ZERO
      DO NL=1,NLOADC
      NN   = ICFIELD(1,NL)
      IDIR = ICFIELD(2,NL)
      IFUNC= ICFIELD(3,NL)
      IAD  = ICFIELD(4,NL)
      IADF = IAD+ITASK*NN/NTHREAD
      IADL = IAD-1+(ITASK+1)*NN/NTHREAD
      IFRA = ICFIELD(7,NL)
      N1FRAM = 0
      IF(IFRA /= 0) N1FRAM = IFRAME(1,IFRA+1)
      IMOVFRAM = 0
      IF (N1FRAM /= 0) THEN
        VN1FRAM(1) = V(1,N1FRAM)
        VN1FRAM(2) = V(2,N1FRAM)
        VN1FRAM(3) = V(3,N1FRAM)
        AN1FRAM(1) = A(1,N1FRAM)
        AN1FRAM(2) = A(2,N1FRAM)
        AN1FRAM(3) = A(3,N1FRAM)
        IMOVFRAM = 1
      ELSE
        VN1FRAM(1) = ZERO
        VN1FRAM(2) = ZERO
        VN1FRAM(3) = ZERO
        AN1FRAM(1) = ZERO
        AN1FRAM(2) = ZERO
        AN1FRAM(3) = ZERO
      ENDIF
      IFLAG=ICFIELD(8,NL)
      ISENS = ICFIELD(6,NL)
      IF(ISENS==0)THEN
          TS=TT
      ELSE
          TS = TT-SENSOR_TAB(ISENS)%TSTART
          IF(TS<ZERO)CYCLE
      ENDIF

      IS_TABULATED = NPC(2*NFUNCT+IFUNC+1)
      IF(IS_TABULATED >= 0) THEN
        VROT  = FAC(1,NL)*FINTER(IFUNC,TS*FAC(2,NL),NPC,TF,DYDX)
      ELSE 
        IS_TABULATED = -IS_TABULATED
        CALL PYTHON_CALL_FUNCT1D(PYTHON, IS_TABULATED,TS*FAC(2,NL), VROT)
        VROT = VROT * FAC(1,NL)
      ENDIF

      VROT2 = VROT*VROT
      X0 = XFRAME(10,IFRA+1)
      Y0 = XFRAME(11,IFRA+1)
      Z0 = XFRAME(12,IFRA+1)
      IF(IDIR == 4) THEN
        X2 = XFRAME(1,IFRA+1)
        Y2 = XFRAME(2,IFRA+1)
        Z2 = XFRAME(3,IFRA+1)
      ELSEIF(IDIR == 5) THEN
        X2 = XFRAME(4,IFRA+1)
        Y2 = XFRAME(5,IFRA+1)
        Z2 = XFRAME(6,IFRA+1)
      ELSE !IF(IDIR == 6) THEN
        X2 = XFRAME(7,IFRA+1)
        Y2 = XFRAME(8,IFRA+1)
        Z2 = XFRAME(9,IFRA+1)
      ENDIF

       IF (IFLAG == 2 .AND. IMOVFRAM == 1) THEN
         DWDT  = FAC(1,NL)*DYDX
#include "vectorize.inc"
         DO J=IADF,IADL
           N1=IABS(IB(J))
           X1 = X(1,N1)-X0
           Y1 = X(2,N1)-Y0
           Z1 = X(3,N1)-Z0
           DIST(1)=X1-(X1*X2+Y1*Y2+Z1*Z2)*X2
           DIST(2)=Y1-(X1*X2+Y1*Y2+Z1*Z2)*Y2
           DIST(3)=Z1-(X1*X2+Y1*Y2+Z1*Z2)*Z2 
           DW(1) = DWDT*X2
           DW(2) = DWDT*Y2
           DW(3) = DWDT*Z2
           AREL(1) = DIST(1)*VROT2 + DW(2) * DIST(3) - DW(3) * DIST(2)
           AREL(2) = DIST(2)*VROT2 + DW(3) * DIST(1) - DW(1) * DIST(3)
           AREL(3) = DIST(3)*VROT2 + DW(1) * DIST(2) - DW(2) * DIST(1)
           CALL RELFRAM_M1(X(1,N1)  ,V(1,N1), AREL , XFRAME(1,IFRA+1), VN1FRAM , AN1FRAM   )
           A(1,N1)=A(1,N1)+AREL(1)
           A(2,N1)=A(2,N1)+AREL(2)
           A(3,N1)=A(3,N1)+AREL(3)
           IF(IB(J)>0)THEN
             VMX=V(1,N1)+HALF*DT2*A(1,N1) ! Supposing A will be no more modified...
             VMY=V(2,N1)+HALF*DT2*A(2,N1)
             VMZ=V(3,N1)+HALF*DT2*A(3,N1) 
             WFEXTT=WFEXTT + MS(N1)*(AREL(1)*VMX+AREL(2)*VMY+AREL(3)*VMZ)*DT12*WEIGHT(N1)
           END IF
         ENDDO
       ELSEIF(IFLAG == 2) THEN
         DWDT  = FAC(1,NL)*DYDX
#include "vectorize.inc"
         DO J=IADF,IADL
           N1=IABS(IB(J))
           X1 = X(1,N1)-X0
           Y1 = X(2,N1)-Y0
           Z1 = X(3,N1)-Z0
           DIST(1)=X1-(X1*X2+Y1*Y2+Z1*Z2)*X2
           DIST(2)=Y1-(X1*X2+Y1*Y2+Z1*Z2)*Y2
           DIST(3)=Z1-(X1*X2+Y1*Y2+Z1*Z2)*Z2 
           DW(1) = DWDT*X2
           DW(2) = DWDT*Y2
           DW(3) = DWDT*Z2
           AREL(1) = DIST(1)*VROT2 + DW(2) * DIST(3) - DW(3) * DIST(2)
           AREL(2) = DIST(2)*VROT2 + DW(3) * DIST(1) - DW(1) * DIST(3)
           AREL(3) = DIST(3)*VROT2 + DW(1) * DIST(2) - DW(2) * DIST(1)
           A(1,N1)=A(1,N1)+AREL(1)
           A(2,N1)=A(2,N1)+AREL(2)
           A(3,N1)=A(3,N1)+AREL(3)
           IF(IB(J)>0)THEN
             VMX=V(1,N1)+HALF*DT2*A(1,N1) ! Supposing A will be no more modified...
             VMY=V(2,N1)+HALF*DT2*A(2,N1)
             VMZ=V(3,N1)+HALF*DT2*A(3,N1) 
             WFEXTT=WFEXTT+MS(N1)*(AREL(1)*VMX+AREL(2)*VMY+AREL(3)*VMZ)*DT12*WEIGHT(N1)
           END IF
         ENDDO
       ELSEIF(IMOVFRAM == 1) THEN
#include "vectorize.inc"
         DO J=IADF,IADL
           N1=IABS(IB(J))
           X1 = X(1,N1)-X0
           Y1 = X(2,N1)-Y0
           Z1 = X(3,N1)-Z0
           DIST(1)=X1-(X1*X2+Y1*Y2+Z1*Z2)*X2
           DIST(2)=Y1-(X1*X2+Y1*Y2+Z1*Z2)*Y2
           DIST(3)=Z1-(X1*X2+Y1*Y2+Z1*Z2)*Z2 
           AREL(1) = DIST(1)*VROT2
           AREL(2) = DIST(2)*VROT2
           AREL(3) = DIST(3)*VROT2
           CALL RELFRAM_M1(X(1,N1)  ,V(1,N1), AREL , XFRAME(1,IFRA+1),VN1FRAM , AN1FRAM   )
           A(1,N1)=A(1,N1)+AREL(1)
           A(2,N1)=A(2,N1)+AREL(2)
           A(3,N1)=A(3,N1)+AREL(3)
           IF(IB(J)>0)THEN
             VMX=V(1,N1)+HALF*DT2*A(1,N1) ! Supposing A will be no more modified...
             VMY=V(2,N1)+HALF*DT2*A(2,N1)
             VMZ=V(3,N1)+HALF*DT2*A(3,N1) 
             WFEXTT=WFEXTT+MS(N1)*(AREL(1)*VMX+AREL(2)*VMY+AREL(3)*VMZ)*DT12*WEIGHT(N1)
           END IF
         ENDDO
       ELSE
#include "vectorize.inc"
         DO J=IADF,IADL
           N1=IABS(IB(J))
           X1 = X(1,N1)-X0
           Y1 = X(2,N1)-Y0
           Z1 = X(3,N1)-Z0
           DIST(1)=X1-(X1*X2+Y1*Y2+Z1*Z2)*X2
           DIST(2)=Y1-(X1*X2+Y1*Y2+Z1*Z2)*Y2
           DIST(3)=Z1-(X1*X2+Y1*Y2+Z1*Z2)*Z2 
           AREL(1) = DIST(1)*VROT2
           AREL(2) = DIST(2)*VROT2
           AREL(3) = DIST(3)*VROT2
           A(1,N1)=A(1,N1)+AREL(1)
           A(2,N1)=A(2,N1)+AREL(2)
           A(3,N1)=A(3,N1)+AREL(3)
           IF(IB(J)>0)THEN
             VMX=V(1,N1)+HALF*DT2*A(1,N1) ! Supposing A will be no more modified...
             VMY=V(2,N1)+HALF*DT2*A(2,N1)
             VMZ=V(3,N1)+HALF*DT2*A(3,N1) 
             WFEXTT=WFEXTT +MS(N1)*(AREL(1)*VMX+AREL(2)*VMY+AREL(3)*VMZ)*DT12*WEIGHT(N1)
           END IF
         ENDDO
       ENDIF
C      /---------------/
       CALL MY_BARRIER
C      /---------------/
      ENDDO
C
!$OMP ATOMIC
      WFEXT = WFEXT + WFEXTT
C
      RETURN
      END
