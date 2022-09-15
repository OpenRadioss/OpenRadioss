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
Chd|  S10DEFO3                      source/elements/solid/solide10/s10defo3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10DEFO3(
     .   PX, PY, PZ, VX, VY, VZ, 
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY,
     .   DZZ, D4, D5, D6, WXX, WYY, WZZ,VOLP,VOLN,RHO,RHOO)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .  VX(MVSIZ,10),VY(MVSIZ,10),VZ(MVSIZ,10),
     .  PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*),VOLP(*),VOLN(*),RHO(*),
     .  RHOO(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,N
C     REAL
      my_real
     .   DT1D2
      my_real 
     .   PXX2,PYY2,PZZ2,PXX2P,PYY2P,PZZ2P,AAA,BBB
C-----------------------------------------------
C   
      IF(ISROT /= 1)THEN                                                                 
        DO I=LFT,LLT
          VOLN(I)=VOLP(I)
          RHOO(I)=RHO(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
c          VOLN(I)=VOLP(I)
          RHOO(I)=RHO(I)
        ENDDO
      ENDIF
      DO I=LFT,LLT
       DXX(I)=PX(I,1)*VX(I,1)+PX(I,2)*VX(I,2)+PX(I,3)*VX(I,3)+PX(I,4)*VX(I,4)+PX(I,5)*VX(I,5)
     .         +PX(I,6)*VX(I,6)+PX(I,7)*VX(I,7)+PX(I,8)*VX(I,8)+PX(I,9)*VX(I,9)+PX(I,10)*VX(I,10)
       DYY(I)=PY(I,1)*VY(I,1)+PY(I,2)*VY(I,2)+PY(I,3)*VY(I,3)+PY(I,4)*VY(I,4)+PY(I,5)*VY(I,5)
     .         +PY(I,6)*VY(I,6)+PY(I,7)*VY(I,7)+PY(I,8)*VY(I,8)+PY(I,9)*VY(I,9)+PY(I,10)*VY(I,10)
       DZZ(I)=PZ(I,1)*VZ(I,1)+PZ(I,2)*VZ(I,2)+PZ(I,3)*VZ(I,3)+PZ(I,4)*VZ(I,4)+PZ(I,5)*VZ(I,5)
     .         +PZ(I,6)*VZ(I,6)+PZ(I,7)*VZ(I,7)+PZ(I,8)*VZ(I,8)+PZ(I,9)*VZ(I,9)+PZ(I,10)*VZ(I,10)
       DXY(I)=PY(I,1)*VX(I,1)+PY(I,2)*VX(I,2)+PY(I,3)*VX(I,3)+PY(I,4)*VX(I,4)+PY(I,5)*VX(I,5)
     .         +PY(I,6)*VX(I,6)+PY(I,7)*VX(I,7)+PY(I,8)*VX(I,8)+PY(I,9)*VX(I,9)+PY(I,10)*VX(I,10)
       DXZ(I)=PZ(I,1)*VX(I,1)+PZ(I,2)*VX(I,2)+PZ(I,3)*VX(I,3)+PZ(I,4)*VX(I,4)+PZ(I,5)*VX(I,5)
     .         +PZ(I,6)*VX(I,6)+PZ(I,7)*VX(I,7)+PZ(I,8)*VX(I,8)+PZ(I,9)*VX(I,9)+PZ(I,10)*VX(I,10)
       DYX(I)=PX(I,1)*VY(I,1)+PX(I,2)*VY(I,2)+PX(I,3)*VY(I,3)+PX(I,4)*VY(I,4)+PX(I,5)*VY(I,5)
     .         +PX(I,6)*VY(I,6)+PX(I,7)*VY(I,7)+PX(I,8)*VY(I,8)+PX(I,9)*VY(I,9)+PX(I,10)*VY(I,10)
       DYZ(I)=PZ(I,1)*VY(I,1)+PZ(I,2)*VY(I,2)+PZ(I,3)*VY(I,3)+PZ(I,4)*VY(I,4)+PZ(I,5)*VY(I,5)
     .         +PZ(I,6)*VY(I,6)+PZ(I,7)*VY(I,7)+PZ(I,8)*VY(I,8)+PZ(I,9)*VY(I,9)+PZ(I,10)*VY(I,10)
       DZX(I)=PX(I,1)*VZ(I,1)+PX(I,2)*VZ(I,2)+PX(I,3)*VZ(I,3)+PX(I,4)*VZ(I,4)+PX(I,5)*VZ(I,5)
     .         +PX(I,6)*VZ(I,6)+PX(I,7)*VZ(I,7)+PX(I,8)*VZ(I,8)+PX(I,9)*VZ(I,9)+PX(I,10)*VZ(I,10)
       DZY(I)=PY(I,1)*VZ(I,1)+PY(I,2)*VZ(I,2)+PY(I,3)*VZ(I,3)+PY(I,4)*VZ(I,4)+PY(I,5)*VZ(I,5)
     .         +PY(I,6)*VZ(I,6)+PY(I,7)*VZ(I,7)+PY(I,8)*VZ(I,8)+PY(I,9)*VZ(I,9)+PY(I,10)*VZ(I,10)
      ENDDO
C
      DT1D2=HALF*DT1
C
      IF(JHBE>=2)THEN
        DO I=LFT,LLT
         DXX(I)  = DXX(I)
     .            -DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
         DYY(I)  = DYY(I)
     .            -DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
         DZZ(I)  = DZZ(I)
     .            -DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
         AAA = DT1D2*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
         DXY(I)  = DXY(I) - AAA
         DYX(I)  = DYX(I) - AAA
         D4(I)   = DXY(I)+DYX(I)
         AAA = DT1D2*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
         DYZ(I)  = DYZ(I) - AAA
         DZY(I)  = DZY(I) - AAA
         D5(I)   = DYZ(I)+DZY(I)
         AAA = DT1D2*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
         DXZ(I)  = DXZ(I) - AAA
         DZX(I)  = DZX(I) - AAA
         D6(I)   = DXZ(I)+DZX(I)
!
         N = 1
         PXX2 = PX(I,N)*PX(I,N)
         PYY2 = PY(I,N)*PY(I,N)
         PZZ2 = PZ(I,N)*PZ(I,N)
         DO N=2,10
           PXX2 = PXX2+PX(I,N)*PX(I,N)
           PYY2 = PYY2+PY(I,N)*PY(I,N)
           PZZ2 = PZZ2+PZ(I,N)*PZ(I,N)
         END DO
         WZZ(I)=DT1*(PYY2*DYX(I)-PXX2*DXY(I))/(PXX2+PYY2)
         WXX(I)=DT1*(PZZ2*DZY(I)-PYY2*DYZ(I))/(PYY2+PZZ2)
         WYY(I)=DT1*(PXX2*DXZ(I)-PZZ2*DZX(I))/(PZZ2+PXX2)
        ENDDO
      ELSE !IF(JHBE>=2)
        DO I=LFT,LLT
         D4(I)   =DXY(I)+DYX(I)
         D5(I)   =DYZ(I)+DZY(I)
         D6(I)   =DXZ(I)+DZX(I)
         WZZ(I)=DT1D2*(DYX(I)-DXY(I))
         WYY(I)=DT1D2*(DXZ(I)-DZX(I))
         WXX(I)=DT1D2*(DZY(I)-DYZ(I))
        ENDDO
      END IF
C
      RETURN
C
      END
Chd|====================================================================
Chd|  S10DEFOT3                     source/elements/solid/solide10/s10defo3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10DEFOT3(
     .   PX, PY, PZ, VX, VY, VZ, 
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY,
     .   DZZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .  VX(MVSIZ,10),VY(MVSIZ,10),VZ(MVSIZ,10),
     .  PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,N
C     REAL
C-----------------------------------------------
C   


        DO I=LFT,LLT
       DXX(I)=PX(I,1)*VX(I,1)+PX(I,2)*VX(I,2)+PX(I,3)*VX(I,3)+PX(I,4)*VX(I,4)+PX(I,5)*VX(I,5)
     .         +PX(I,6)*VX(I,6)+PX(I,7)*VX(I,7)+PX(I,8)*VX(I,8)+PX(I,9)*VX(I,9)+PX(I,10)*VX(I,10)
       DYY(I)=PY(I,1)*VY(I,1)+PY(I,2)*VY(I,2)+PY(I,3)*VY(I,3)+PY(I,4)*VY(I,4)+PY(I,5)*VY(I,5)
     .         +PY(I,6)*VY(I,6)+PY(I,7)*VY(I,7)+PY(I,8)*VY(I,8)+PY(I,9)*VY(I,9)+PY(I,10)*VY(I,10)
       DZZ(I)=PZ(I,1)*VZ(I,1)+PZ(I,2)*VZ(I,2)+PZ(I,3)*VZ(I,3)+PZ(I,4)*VZ(I,4)+PZ(I,5)*VZ(I,5)
     .         +PZ(I,6)*VZ(I,6)+PZ(I,7)*VZ(I,7)+PZ(I,8)*VZ(I,8)+PZ(I,9)*VZ(I,9)+PZ(I,10)*VZ(I,10)
       DXY(I)=PY(I,1)*VX(I,1)+PY(I,2)*VX(I,2)+PY(I,3)*VX(I,3)+PY(I,4)*VX(I,4)+PY(I,5)*VX(I,5)
     .         +PY(I,6)*VX(I,6)+PY(I,7)*VX(I,7)+PY(I,8)*VX(I,8)+PY(I,9)*VX(I,9)+PY(I,10)*VX(I,10)
       DXZ(I)=PZ(I,1)*VX(I,1)+PZ(I,2)*VX(I,2)+PZ(I,3)*VX(I,3)+PZ(I,4)*VX(I,4)+PZ(I,5)*VX(I,5)
     .         +PZ(I,6)*VX(I,6)+PZ(I,7)*VX(I,7)+PZ(I,8)*VX(I,8)+PZ(I,9)*VX(I,9)+PZ(I,10)*VX(I,10)
       DYX(I)=PX(I,1)*VY(I,1)+PX(I,2)*VY(I,2)+PX(I,3)*VY(I,3)+PX(I,4)*VY(I,4)+PX(I,5)*VY(I,5)
     .         +PX(I,6)*VY(I,6)+PX(I,7)*VY(I,7)+PX(I,8)*VY(I,8)+PX(I,9)*VY(I,9)+PX(I,10)*VY(I,10)
       DYZ(I)=PZ(I,1)*VY(I,1)+PZ(I,2)*VY(I,2)+PZ(I,3)*VY(I,3)+PZ(I,4)*VY(I,4)+PZ(I,5)*VY(I,5)
     .         +PZ(I,6)*VY(I,6)+PZ(I,7)*VY(I,7)+PZ(I,8)*VY(I,8)+PZ(I,9)*VY(I,9)+PZ(I,10)*VY(I,10)
       DZX(I)=PX(I,1)*VZ(I,1)+PX(I,2)*VZ(I,2)+PX(I,3)*VZ(I,3)+PX(I,4)*VZ(I,4)+PX(I,5)*VZ(I,5)
     .         +PX(I,6)*VZ(I,6)+PX(I,7)*VZ(I,7)+PX(I,8)*VZ(I,8)+PX(I,9)*VZ(I,9)+PX(I,10)*VZ(I,10)
       DZY(I)=PY(I,1)*VZ(I,1)+PY(I,2)*VZ(I,2)+PY(I,3)*VZ(I,3)+PY(I,4)*VZ(I,4)+PY(I,5)*VZ(I,5)
     .         +PY(I,6)*VZ(I,6)+PY(I,7)*VZ(I,7)+PY(I,8)*VZ(I,8)+PY(I,9)*VZ(I,9)+PY(I,10)*VZ(I,10)
      ENDDO
C
      RETURN
C
      END
