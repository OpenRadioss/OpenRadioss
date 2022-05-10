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
Chd|  INTANL_QD                     source/fluid/intanl_qd.F      
Chd|-- called by -----------
Chd|        MASS_FLUID_QD                 source/fluid/mass-fluid_qd.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTANL_QD(X1,  Y1,  Z1,  X2,  Y2,  Z2,  X3,  Y3,  Z3, 
     .                     X4,  Y4,  Z4,  XP,  YP,  ZP, NRX, NRY, NRZ,
     .                     AREA,RVLH,RVLG,JEL, IEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEL, JEL
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4, Z4, XP, YP, ZP,
     .        NRX, NRY, NRZ, AREA, RVLH, RVLG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .        VX1, VY1, VZ1, VX2, VY2, VZ2, S1, S12, S2, NR1, NR2, 
     .        X0, Y0, Z0, KSI(5), ETA(5), DKSI(4), DETA(4), R(5),
     .        XLS, YLS, ZLS, S(4), V, FLN, ARG,
     .        D2, L12, L22, L32, L42, LM2
      my_real CS(4), SN(4)
C
      X0=QUART*(X1+X2+X3+X4)
      Y0=QUART*(Y1+Y2+Y3+Y4)
      Z0=QUART*(Z1+Z2+Z3+Z4)
C
C SIMPLIFICATION SI SOURCE LOIN DU POINT P
      D2 =(X0-XP)**2+(Y0-YP)**2+(Z0-ZP)**2
      L12=(X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
      L22=(X3-X2)**2+(Y3-Y2)**2+(Z3-Z2)**2
      L32=(X4-X3)**2+(Y4-Y3)**2+(Z4-Z3)**2
      L42=(X1-X4)**2+(Y1-Y4)**2+(Z1-Z4)**2
      LM2=MAX(L12,L22,L32,L42)

      IF(D2.GT.VINGT5*LM2) THEN  
        RVLG=AREA/SQRT(D2)
        RVLH=AREA*(NRX*(X0-XP)+NRY*(Y0-YP)+NRZ*(Z0-ZP))/(D2**TDEMI)	  
      ELSE
C COORDONNEES LOCALES
        VX1=X2-X1
        VY1=Y2-Y1
        VZ1=Z2-Z1
        VX2=X4-X1
        VY2=Y4-Y1
        VZ2=Z4-Z1
C
        S1 =VX1*VX1+VY1*VY1+VZ1*VZ1
        S12=VX1*VX2+VY1*VY2+VZ1*VZ2
        NR1=SQRT(S1)
C     
        VX2=VX2-S12/S1*VX1
        VY2=VY2-S12/S1*VY1
        VZ2=VZ2-S12/S1*VZ1
C
        S2=VX2*VX2+VY2*VY2+VZ2*VZ2
        NR2=SQRT(S2)
        VX1=VX1/NR1
        VY1=VY1/NR1
        VZ1=VZ1/NR1
        VX2=VX2/NR2
        VY2=VY2/NR2
        VZ2=VZ2/NR2
C
        XLS=(XP-X0)*VX1+(YP-Y0)*VY1+(ZP-Z0)*VZ1
        YLS=(XP-X0)*VX2+(YP-Y0)*VY2+(ZP-Z0)*VZ2
        ZLS=(XP-X0)*NRX+(YP-Y0)*NRY+(ZP-Z0)*NRZ

        KSI(1)=(X1-X0)*VX1+(Y1-Y0)*VY1+(Z1-Z0)*VZ1
        ETA(1)=(X1-X0)*VX2+(Y1-Y0)*VY2+(Z1-Z0)*VZ2
        KSI(2)=(X2-X0)*VX1+(Y2-Y0)*VY1+(Z2-Z0)*VZ1
        ETA(2)=(X2-X0)*VX2+(Y2-Y0)*VY2+(Z2-Z0)*VZ2
        KSI(3)=(X3-X0)*VX1+(Y3-Y0)*VY1+(Z3-Z0)*VZ1
        ETA(3)=(X3-X0)*VX2+(Y3-Y0)*VY2+(Z3-Z0)*VZ2
        KSI(4)=(X4-X0)*VX1+(Y4-Y0)*VY1+(Z4-Z0)*VZ1
        ETA(4)=(X4-X0)*VX2+(Y4-Y0)*VY2+(Z4-Z0)*VZ2
        KSI(5)=KSI(1)
        ETA(5)=ETA(1)

        DKSI(1)=KSI(2)-KSI(1)
        DKSI(2)=KSI(3)-KSI(2)
        DKSI(3)=KSI(4)-KSI(3)
        DKSI(4)=KSI(1)-KSI(4)
        DETA(1)=ETA(2)-ETA(1)
        DETA(2)=ETA(3)-ETA(2)
        DETA(3)=ETA(4)-ETA(3)
        DETA(4)=ETA(1)-ETA(4)
        R(1)=SQRT((XP-X1)**2+(YP-Y1)**2+(ZP-Z1)**2)
        R(2)=SQRT((XP-X2)**2+(YP-Y2)**2+(ZP-Z2)**2)
        R(3)=SQRT((XP-X3)**2+(YP-Y3)**2+(ZP-Z3)**2)
        R(4)=SQRT((XP-X4)**2+(YP-Y4)**2+(ZP-Z4)**2)
        S(1)=SQRT(L12)
        S(2)=SQRT(L22)
        S(3)=SQRT(L32)
        S(4)=SQRT(L42)
        R(5)=R(1)
	  
        DO I=1,4
          CS(I)=DKSI(I)/S(I)
          SN(I)=DETA(I)/S(I)
        ENDDO
C
        RVLH=ZERO
C INTEGRALE DOUBLE COUCHE
        IF (ZLS.NE.ZERO) THEN
           DO I=1,4
              J=I+1
              RVLH=RVLH
     .       +ATAN((DETA(I)*((XLS-KSI(I))**2+ZLS**2)-DKSI(I)*(XLS-KSI(I))*(YLS-ETA(I)))/(R(I)*ZLS*DKSI(I)))
     .       -ATAN((DETA(I)*((XLS-KSI(J))**2+ZLS**2)-DKSI(I)*(XLS-KSI(J))*(YLS-ETA(J)))/(R(J)*ZLS*DKSI(I)))
           ENDDO
        ENDIF
C
C INTEGRALE SIMPLE COUCHE
        RVLG=ZERO
        DO I=1,4
           J=I+1
           V=(XLS-KSI(I))*SN(I)-(YLS-ETA(I))*CS(I)
           ARG=(R(I)+R(J)-S(I))/(R(I)+R(J)+S(I))
           IF (ARG.GT.ZERO) THEN
              FLN=-LOG(ARG)
              RVLG=RVLG+V*FLN
           ELSE
              WRITE(6,'(A,E13.5)') '? ARG=',ARG
           ENDIF
        ENDDO
        RVLG=-RVLG+ZLS*RVLH

      ENDIF	 
C
      RETURN
      END
