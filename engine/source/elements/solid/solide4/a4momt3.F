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
Chd|  A4MOMT3                       source/elements/solid/solide4/a4momt3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|        A4MOMTN3                      source/elements/solid/solide4/a4momtn3.F
Chd|        UPWIND4                       source/elements/solid/solide4/upwind4.F
Chd|====================================================================
      SUBROUTINE A4MOMT3(PM,RHO,VOL,
     .   X1,X2,X3,X4,
     .   Y1,Y2,Y3,Y4,
     .   Z1,Z2,Z3,Z4,
     .   VX1,VX2,VX3,VX4,
     .   VY1,VY2,VY3,VY4,
     .   VZ1,VZ2,VZ3,VZ4,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   PX1,PX2,PX3,PX4,
     .   PY1,PY2,PY3,PY4,
     .   PZ1,PZ2,PZ3,PZ4,
     .   DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,
     .   VDX1,VDX2,VDX3,VDX4,
     .   VDY1,VDY2,VDY3,VDY4,
     .   VDZ1,VDZ2,VDZ3,VDZ4,
     .   VDX,VDY,VDZ,
     .   DELTAX,VIS,
     .   MAT,RX,RY,RZ,SX,SY,SZ,TX,TY,TZ)
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scrupw_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   PM(NPROPM,*), RHO(*),VOL(*),
     .   X1(*),X2(*),X3(*),X4(*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),
     .   Z1(*),Z2(*),Z3(*),Z4(*),
     .   VX1(*),VX2(*),VX3(*),VX4(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ4(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   PX1(*),PX2(*),PX3(*),PX4(*),
     .   PY1(*),PY2(*),PY3(*),PY4(*),
     .   PZ1(*),PZ2(*),PZ3(*),PZ4(*),    
     .   DXX(*),DXY(*),DXZ(*),
     .   DYX(*),DYY(*),DYZ(*),
     .   DZX(*),DZY(*),DZZ(*),
     .   VDX1(*),VDX2(*),VDX3(*),VDX4(*),
     .   VDY1(*),VDY2(*),VDY3(*),VDY4(*),
     .   VDZ1(*),VDZ2(*),VDZ3(*),VDZ4(*),
     .   VDX(*),VDY(*),VDZ(*),
     .   DELTAX(*),VIS(*),
     .   RX(*) , RY(*) , RZ(*) ,
     .   SX(*) , SY(*) , SZ(*) ,
     .   TX(*) , TY(*) , TZ(*) 
      INTEGER MAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   V12(MVSIZ,3),V34(MVSIZ,3),V13(MVSIZ,3),V14(MVSIZ,3),
     .   V24(MVSIZ,3),V23(MVSIZ,3),
     .   F1(MVSIZ),F2(MVSIZ),F3(MVSIZ),
     .   A1(MVSIZ),A2(MVSIZ),A3(MVSIZ),A4(MVSIZ),GAM(MVSIZ),
     .   R(MVSIZ,3),S(MVSIZ,3),T(MVSIZ,3),FAC,
     .   AJV(MVSIZ),AJ0(MVSIZ,3),AJ1(MVSIZ,3),AJ2(MVSIZ,3),AJ3(MVSIZ,3)
      INTEGER I,J
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
      IF(MTN==11)RETURN
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------      
      IF(UPWM>1)THEN
      !      DO I=LFT,LLT
      !       V12(1,I)=X2(I)-X1(I)
      !       V12(2,I)=Y2(I)-Y1(I)
      !       V12(3,I)=Z2(I)-Z1(I)
      !       V13(1,I)=X3(I)-X1(I)
      !       V13(2,I)=Y3(I)-Y1(I)
      !       V13(3,I)=Z3(I)-Z1(I)
      !       V14(1,I)=X4(I)-X1(I)
      !       V14(2,I)=Y4(I)-Y1(I)
      !       V14(3,I)=Z4(I)-Z1(I)
      !       V24(1,I)=X4(I)-X2(I)
      !       V24(2,I)=Y4(I)-Y2(I)
      !       V24(3,I)=Z4(I)-Z2(I)
      !       V23(1,I)=X3(I)-X2(I)
      !       V23(2,I)=Y3(I)-Y2(I)
      !       V23(3,I)=Z3(I)-Z2(I)
      !       V34(1,I)=X4(I)-X3(I)
      !       V34(2,I)=Y4(I)-Y3(I)
      !       V34(3,I)=Z4(I)-Z3(I)
      !      ENDDO
      !      DO J=1,3
      !       DO I=LFT,LLT
      !        R(J,I)=0.5*V34(J,I)
      !        S(J,I)=0.5*V12(J,I)
      !        T(J,I)=0.25*(V13(J,I)+V14(J,I)+V24(J,I)+V23(J,I))
      !       ENDDO
      !      ENDDO
      ENDIF
      
      ! CALCUL DE LA FORCE DE TRANSPORT AU CENTRE
      ! STANDARD UPWIND UTILISANT LES DONNEES MATERIAUX
      IF(UPWM<2 .OR.MTN==11)THEN
        IF(MTN==11.AND.UPWM>1)THEN
          ! VDX,VDY,VDZ EST CALCULE DANS M11VS3
          DO I=LFT,LLT
             R(I,1)=RX(I)
             R(I,2)=RY(I)
             R(I,3)=RZ(I)
             S(I,1)=SX(I)
             S(I,2)=SY(I)
             S(I,3)=SZ(I)
             T(I,1)=TX(I)
             T(I,2)=TY(I)
             T(I,3)=TZ(I)
          ENDDO
          CALL UPWIND4(RHO,VIS,VDX,VDY,VDZ,R,S,T,GAM)
          DO I=LFT,LLT
            FAC=GAM(I)
            A1(I) = FAC*(PX1(I)*VDX(I)+PY1(I)*VDY(I)+PZ1(I)*VDZ(I))
            A2(I) = FAC*(PX2(I)*VDX(I)+PY2(I)*VDY(I)+PZ2(I)*VDZ(I))
            A3(I) = FAC*(PX3(I)*VDX(I)+PY3(I)*VDY(I)+PZ3(I)*VDZ(I))
            A4(I) = FAC*(PX4(I)*VDX(I)+PY4(I)*VDY(I)+PZ4(I)*VDZ(I))
          ENDDO
          
        ELSE!IF(MTN==11.AND.UPWM>1)
        
          IF(UPWM==ZERO)THEN
            DO I=LFT,LLT
              GAM(I)= PM(15,MAT(I))
            ENDDO
          ELSE
            DO I=LFT,LLT
              GAM(I)= CUPWM
            ENDDO
          ENDIF
          DO I=LFT,LLT
           A1(I) = PX1(I)*VDX(I)+PY1(I)*VDY(I)+PZ1(I)*VDZ(I)
           A2(I) = PX2(I)*VDX(I)+PY2(I)*VDY(I)+PZ2(I)*VDZ(I)
           A3(I) = PX3(I)*VDX(I)+PY3(I)*VDY(I)+PZ3(I)*VDZ(I)
           A4(I) = PX4(I)*VDX(I)+PY4(I)*VDY(I)+PZ4(I)*VDZ(I)
           A1(I) = SIGN(GAM(I),A1(I))
           A2(I) = SIGN(GAM(I),A2(I))
           A3(I) = SIGN(GAM(I),A3(I))
           A4(I) = SIGN(GAM(I),A4(I))
          ENDDO
          
        ENDIF !IF(MTN==11.AND.UPWM>1)
       
         DO I=LFT,LLT
           FAC = FOURTH*RHO(I)*VOL(I)
           F1(I) = (VDX(I)*DXX(I)+VDY(I)*DXY(I)+VDZ(I)*DXZ(I))*FAC
           F2(I) = (VDX(I)*DYX(I)+VDY(I)*DYY(I)+VDZ(I)*DYZ(I))*FAC
           F3(I) = (VDX(I)*DZX(I)+VDY(I)*DZY(I)+VDZ(I)*DZZ(I))*FAC
         ENDDO
       
         DO I=LFT,LLT
          F11(I) = F11(I) - (ONE+A1(I))*F1(I)
          F12(I) = F12(I) - (ONE+A2(I))*F1(I)
          F13(I) = F13(I) - (ONE+A3(I))*F1(I)
          F14(I) = F14(I) - (ONE+A4(I))*F1(I)

          F21(I) = F21(I) - (ONE+A1(I))*F2(I)
          F22(I) = F22(I) - (ONE+A3(I))*F2(I)
          F23(I) = F23(I) - (ONE+A3(I))*F2(I)
          F24(I) = F24(I) - (ONE+A4(I))*F2(I)

          F31(I) = F31(I) - (ONE+A1(I))*F3(I)
          F32(I) = F32(I) - (ONE+A3(I))*F3(I)
          F33(I) = F33(I) - (ONE+A3(I))*F3(I)
          F34(I) = F34(I) - (ONE+A4(I))*F3(I)
         ENDDO

      ELSE ! IF(UPWM<2 .OR.MTN==11)

      ! SUPG OU TG
      ! FORMULATION DE LA FORCE DE TRANSPORT
      ! ON EVALUE <PHI,UJ*DUI/DXJ> DIRECTEMENT AUX NOEUDS 

        DO I=LFT,LLT
           R(I,1)=RX(I)-SX(I)
           R(I,2)=RY(I)-SY(I)
           R(I,3)=RZ(I)-SZ(I)
           S(I,1)=RX(I)-TX(I)
           S(I,2)=RY(I)-TY(I)
           S(I,3)=RZ(I)-TZ(I)
           T(I,1)=RX(I)
           T(I,2)=RY(I)
           T(I,3)=RZ(I)
        ENDDO
        !    CURRENT DEFINITION FOR R,S,T (Node 1)
        !    +------+
        !    ^     /+
        !    |    / +
        !   T|   /  +
        !    |  /S  +
        !    | /    +
        !    |/     +
        !    +----->+
        !   1   R
        CALL A4MOMTN3(
     .     RHO,VOL,VIS,R,S,T,DELTAX,VDX,VDY,VDZ,
     .     DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,
     .     PX1,PY1,PZ1,VDX1,VDY1,VDZ1,F11,F21,F31)
     
        DO I=LFT,LLT
           R(I,1)=SX(I)-TX(I)    
           R(I,2)=SY(I)-TY(I)
           R(I,3)=SZ(I)-TZ(I)
           S(I,1)=SX(I)
           S(I,2)=SY(I)
           S(I,3)=SZ(I)
           T(I,1)=SX(I)-RX(I)
           T(I,2)=SY(I)-RY(I)
           T(I,3)=SZ(I)-RZ(I)
        ENDDO
        !    CURRENT DEFINITION FOR R,S,T (Node 2)
        !    +------+
        !    +\     ^
        !    + \    | R
        !    +  \S  |
        !    +   \  | 
        !    +    \ |
        !    +     \|
        !    +<-----0
        !       T    2 
        CALL A4MOMTN3(
     .     RHO,VOL,VIS,R,S,T,DELTAX,VDX,VDY,VDZ,
     .     DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,
     .     PX2,PY2,PZ2,VDX2,VDY2,VDZ2,F12,F22,F32)
     
        DO I=LFT,LLT
           R(I,1)=TX(I)
           R(I,2)=TY(I)
           R(I,3)=TZ(I)
           S(I,1)=TX(I)-RX(I)
           S(I,2)=TY(I)-RY(I)
           S(I,3)=TZ(I)-RZ(I)
           T(I,1)=TX(I)-SX(I)
           T(I,2)=TY(I)-SY(I)
           T(I,3)=TZ(I)-SZ(I)
        ENDDO
        !    CURRENT DEFINITION FOR R,S,T
        !       R
        !    +<-----O 3
        !    +     /|
        !    +    / |
        !    +   /S |
        !    +  /   |T
        !    + /    |
        !    +/     |
        !    +------+
        !             
        CALL A4MOMTN3(
     .     RHO,VOL,VIS,R,S,T,DELTAX,VDX,VDY,VDZ,
     .     DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,
     .     PX3,PY3,PZ3,VDX3,VDY3,VDZ3,F13,F23,F33)
     
        DO I=LFT,LLT
           R(I,1)=RX(I)
           R(I,2)=RY(I)
           R(I,3)=RZ(I)
           S(I,1)=SX(I)
           S(I,2)=SY(I)
           S(I,3)=SZ(I)
           T(I,1)=TX(I)
           T(I,2)=TY(I)
           T(I,3)=TZ(I)
        ENDDO
        !    CURRENT DEFINITION FOR R,S,T
        !   4    T
        !    O------+
        !    |\     +
        !    | \    +
        !  R |  \S  +
        !    |   \  +
        !    |    \ +
        !    |     \+
        !    +------+
        !            
        CALL A4MOMTN3(
     .     RHO,VOL,VIS,R,S,T,DELTAX,VDX,VDY,VDZ,
     .     DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,
     .     PX4,PY4,PZ4,VDX4,VDY4,VDZ4,F14,F24,F34)
     
     
      ENDIF!IF(UPWM<2 .OR.MTN==11)

      RETURN
      END
      
