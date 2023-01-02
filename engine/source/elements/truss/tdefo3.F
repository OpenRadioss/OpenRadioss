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
Chd|  TDEFO3                        source/elements/truss/tdefo3.F
Chd|-- called by -----------
Chd|        TFORC3                        source/elements/truss/tforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TDEFO3(V   ,OFFG ,OFF ,NC1 ,NC2,
     .                  EPS ,AL   ,VX1 ,VX2 ,VY1,
     .                  VY2 ,VZ1  ,VZ2 ,EX  ,EY ,
     .                  EZ  ,X1   ,X2  ,Y1  ,Y2 ,
     .                  Z1  ,Z2   )
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(MVSIZ),NC2(MVSIZ)
      my_real
     .   V(3,*),OFFG(MVSIZ),OFF(MVSIZ),EPS(MVSIZ),AL(MVSIZ),
     .   VX1(MVSIZ),VX2(MVSIZ),VY1(MVSIZ),VY2(MVSIZ),VZ1(MVSIZ),
     .   VZ2(MVSIZ),EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ),X1(MVSIZ),X2(MVSIZ),
     .   Y1(MVSIZ),Y2(MVSIZ),Z1(MVSIZ),Z2(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   V1(MVSIZ),V2(MVSIZ),RATIO(MVSIZ),OFF_L
C-----------------------------------------------      
      DO I=LFT,LLT
        VX1(I)=V(1,NC1(I))
        VY1(I)=V(2,NC1(I))
        VZ1(I)=V(3,NC1(I))
        VX2(I)=V(1,NC2(I))
        VY2(I)=V(2,NC2(I))
        VZ2(I)=V(3,NC2(I))
      ENDDO
c
      DO I=LFT,LLT
        EX(I)=X2(I)-X1(I)
        EY(I)=Y2(I)-Y1(I)
        EZ(I)=Z2(I)-Z1(I)
      ENDDO
c
      DO I=LFT,LLT
        AL(I)=SQRT(EX(I)*EX(I)+EY(I)*EY(I)+EZ(I)*EZ(I))
      ENDDO
c
      DO I=LFT,LLT
        IF (AL(I) /= ZERO) THEN
          EX(I) = EX(I)/AL(I)
          EY(I) = EY(I)/AL(I)
          EZ(I) = EZ(I)/AL(I)
        ELSE
          EX(I) = ZERO
          EY(I) = ZERO
          EZ(I) = ZERO                    
        ENDIF
      ENDDO
c
      DO I=LFT,LLT
        V1(I)=EX(I)*VX1(I)+EY(I)*VY1(I)+EZ(I)*VZ1(I)
        V2(I)=EX(I)*VX2(I)+EY(I)*VY2(I)+EZ(I)*VZ2(I)
      ENDDO
c
      DO I=LFT,LLT
        IF (AL(I) /= ZERO) THEN
          EPS(I)= (V2(I)-V1(I))/AL(I)     
          RATIO(I) = ( (VX2(I)-VX1(I))*(VX2(I)-VX1(I))
     .                +(VY2(I)-VY1(I))*(VY2(I)-VY1(I))
     .                +(VZ2(I)-VZ1(I))*(VZ2(I)-VZ1(I)) )
     .               /(AL(I)*AL(I))
         ELSE
           EPS(I)   = ZERO
           RATIO(I) = ZERO
         ENDIF
      ENDDO
c
      DO I=LFT,LLT
        EPS(I)= EPS(I) + HALF*DT1*( EPS(I)*EPS(I) - RATIO(I) )
      ENDDO
c
      OFF_L = ZERO
      DO I=LFT,LLT
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
        OFF(I) = MAX(ZERO,OFF(I))
        OFF_L  = MIN(OFF_L,OFFG(I))
      ENDDO
c
      IF (OFF_L < ZERO) THEN
        DO I=LFT,LLT
         IF (OFFG(I) < ZERO) THEN
           EPS(I)=ZERO
         ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------      
      RETURN
      END
