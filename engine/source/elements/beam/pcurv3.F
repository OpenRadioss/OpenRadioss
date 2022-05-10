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
Chd|  PCURV3                        source/elements/beam/pcurv3.F 
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PCURV3(R      ,GEO    ,OFFG   ,OFF    ,EXX  ,
     .                  EXY    ,EXZ    ,KXX    ,KYY    ,KZZ  ,
     .                  AL     ,NC1    ,NC2    ,NC3    ,RX1G ,
     .                  RX2G   ,RY1G   ,RY2G   ,RZ1G   ,RZ2G ,
     .                  E1X    ,E1Y    ,E1Z    ,E2X    ,E2Y  ,
     .                  E2Z    ,E3X    ,E3Y    ,E3Z    ,PID  )
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),PID(MVSIZ)
      my_real
     .   R(3,*), GEO(NPROPG,*),
     .   OFFG(*),OFF(*),AL(MVSIZ),EXX(MVSIZ),EXY(MVSIZ),EXZ(MVSIZ),
     .   KXX(MVSIZ),KYY(MVSIZ),KZZ(MVSIZ),RX1G(MVSIZ),RX2G(MVSIZ),
     .   RY1G(MVSIZ),RY2G(MVSIZ),RZ1G(MVSIZ),RZ2G(MVSIZ),
     .   RX1(MVSIZ),RX2(MVSIZ),RY1(MVSIZ),RY2(MVSIZ),RZ1(MVSIZ),RZ2(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),E2Y(MVSIZ),
     .   E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, IRX, IR1Y, IR1Z,
     .   IR2Y, IR2Z, IRY, IRZ, J
      my_real
     .   RXAV(MVSIZ), RYAV(MVSIZ), RZAV(MVSIZ),EXX00(MVSIZ), EXY00(MVSIZ), 
     .   EXZ00(MVSIZ), EXY0(MVSIZ), EXZ0(MVSIZ),RZ10(MVSIZ),
     .   RY10(MVSIZ), RZ20(MVSIZ), RY20(MVSIZ), EXX00PHI,EXY00PHI, 
     .   EXZ00PHI, EXY0PHI, EXZ0PHI, RX10PHI, RY10PHI, RZ10PHI, 
     .   RX20PHI, RY20PHI, RZ20PHI,OFF_L,DT05   
C-----------------------------------------------
      DO I=LFT,LLT
        RX1(I)=E1X(I)*RX1G(I)+E1Y(I)*RY1G(I)+E1Z(I)*RZ1G(I)
        RY1(I)=E2X(I)*RX1G(I)+E2Y(I)*RY1G(I)+E2Z(I)*RZ1G(I)
        RZ1(I)=E3X(I)*RX1G(I)+E3Y(I)*RY1G(I)+E3Z(I)*RZ1G(I)
        RX2(I)=E1X(I)*RX2G(I)+E1Y(I)*RY2G(I)+E1Z(I)*RZ2G(I)
        RY2(I)=E2X(I)*RX2G(I)+E2Y(I)*RY2G(I)+E2Z(I)*RZ2G(I)
        RZ2(I)=E3X(I)*RX2G(I)+E3Y(I)*RY2G(I)+E3Z(I)*RZ2G(I)
      ENDDO
C---------------------------------
C     SECOND ORDRE TRANSLATIONS ROTATIONS
C---------------------------------
      DT05=UNDEMI*DT1
      IF (IMPL_S > 0 .AND. ISMDISP > 0) DT05=ZERO
C
      DO I=LFT,LLT
        RXAV(I)= UNDEMI*DT05*(RX1(I)+RX2(I))
        EXX00(I)=EXX(I)
        EXY00(I)=EXY(I)
        EXZ00(I)=EXZ(I)
C
        EXY0(I)=DT05*EXY(I)
        EXZ0(I)=DT05*EXZ(I)
        EXX(I)=EXX(I)-(EXY0(I)*EXY00(I)+EXZ0(I)*EXZ00(I))
        EXY(I)=EXY(I)+EXZ0(I)*EXX00(I)
        EXZ(I)=EXZ(I)+EXY0(I)*EXX00(I)
C ... this looks suspicious.
        RZ10(I)=RZ1(I)
        RY10(I)=RY1(I)
        RZ20(I)=RZ2(I)
        RY20(I)=RY2(I)
        RX1(I)=RX1(I)-EXY0(I)*RY10(I)-EXZ0(I)*RZ10(I)
        RY1(I)=RY1(I)-RXAV(I)*(RZ10(I)-EXY(I))
        RZ1(I)=RZ1(I)+RXAV(I)*(RY10(I)+EXZ(I))
        RX2(I)=RX2(I)-EXY0(I)*RY20(I)-EXZ0(I)*RZ20(I)
        RY2(I)=RY2(I)-RXAV(I)*(RZ20(I)-EXY(I))
        RZ2(I)=RZ2(I)+RXAV(I)*(RY20(I)+EXZ(I))
      ENDDO
C---------------------------------------------------
C     LIBERATION DES ROTATIONS
C---------------------------------------------------
      DO I=LFT,LLT
        IG=PID(I)
        IRX =NINT(GEO(7 ,IG))
        IR1Y=NINT(GEO(8 ,IG))
        IR1Z=NINT(GEO(9 ,IG))
        IR2Y=NINT(GEO(10,IG))
        IR2Z=NINT(GEO(11,IG))
        IRY =MIN(1,IR1Y+IR2Y)
        IRZ =MIN(1,IR1Z+IR2Z)
        RX1(I)=RX1(I)*IRX
        RY1(I)=RY1(I)*IRY
        RZ1(I)=RZ1(I)*IRZ
        RX2(I)=RX2(I)*IRX
        RY2(I)=RY2(I)*IRY
        RZ2(I)=RZ2(I)*IRZ
        EXZ(I)=EXZ(I)*IRY
        EXY(I)=EXY(I)*IRZ
C ... !!!!!!!
        RY1(I)=IR1Y*RY1(I)
     +         -(UN -IR1Y)*(TDEMI*EXZ(I)+UNDEMI*RY2(I))
        RY2(I)=IR2Y*RY2(I)
     +         -(UN -IR2Y)*(TDEMI*EXZ(I)+UNDEMI*RY1(I))
        RZ1(I)=IR1Z*RZ1(I)
     +         +(UN-IR1Z)*(TDEMI*EXY(I)-UNDEMI*RZ2(I))
        RZ2(I)=IR2Z*RZ2(I)
     +         +(UN -IR2Z)*(TDEMI*EXY(I)-UNDEMI*RZ1(I))
      ENDDO
C
      DO I=LFT,LLT
        KXX(I)=(RX2(I)-RX1(I))/AL(I)
        KYY(I)=(RY2(I)-RY1(I))/AL(I)
        KZZ(I)=(RZ2(I)-RZ1(I))/AL(I)
      ENDDO
C
      DO I=LFT,LLT
        RXAV(I)=RX1(I)+RX2(I)
        RZAV(I)=RZ1(I)+RZ2(I)
        RYAV(I)=RY1(I)+RY2(I)
      ENDDO
C---------------------------------
      DO I=LFT,LLT
        EXZ(I)=EXZ(I) + UNDEMI*RYAV(I)
        EXY(I)=EXY(I) - UNDEMI*RZAV(I)
      ENDDO
C----------------------------
C     OFF
C----------------------------
      OFF_L = ZERO
      DO I=LFT,LLT
        OFF(I) = MIN(UN,ABS(OFFG(I)))
        OFF(I) = MAX(ZERO,OFF(I))
        OFF_L  = MIN(OFF_L,OFFG(I))
      ENDDO
      IF (OFF_L < ZERO) THEN
        DO I=LFT,LLT
          IF (OFFG(I) < ZERO) THEN
            EXX(I)=ZERO
            EXZ(I)=ZERO
            EXY(I)=ZERO
            KXX(I)=ZERO
            KYY(I)=ZERO
            KZZ(I)=ZERO   
          ENDIF
        ENDDO
      ENDIF
C---------------------------------
      RETURN
      END
