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
Chd|  S10MALLA3                     source/elements/solid/solide10/s10mall3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10MALLA3(OFFG ,OFF  ,WXX,WYY,WZZ,
     .                     WXXG,WYYG,WZZG,WIP )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   OFFG(*),OFF(*),WXX(*),WYY(*),WZZ(*),
     .   WXXG(*),WYYG(*),WZZG(*),WIP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C     REAL
      my_real
     .   X , Y, Z ,DV
C-----------------------------------------------
C-----------------------------
C     ROTATION RBY DES COORDONNEES POUR SMALL STRAIN
C-----------------------------
      IF(ISMSTR<=4.AND.JLAG>0)THEN
       DO I=LFT,LLT
         WXXG(I)=WXXG(I)+WIP*WXX(I)
         WYYG(I)=WYYG(I)+WIP*WYY(I)
         WZZG(I)=WZZG(I)+WIP*WZZ(I)
       ENDDO
      ELSEIF(ISMSTR==11)THEN
       DO I=LFT,LLT
         WXXG(I)=WXXG(I)+WIP*WXX(I)
         WYYG(I)=WYYG(I)+WIP*WYY(I)
         WZZG(I)=WZZG(I)+WIP*WZZ(I)
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  S10MALLB3                     source/elements/solid/solide10/s10mall3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10MALLB3(SAV, OFFG ,WXX,WYY,WZZ,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   OFFG(*),WXX(*),WYY(*),WZZ(*)
      DOUBLE PRECISION 
     .   SAV(NEL,30)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C     REAL
      DOUBLE PRECISION 
     .   X , Y, Z 
C-----------------------------------------------
C-----------------------------
C     ROTATION RBY DES COORDONNEES POUR SMALL STRAIN
C-----------------------------
C-----------------implicit sm disp option---
      IF (ISMDISP>0) THEN
      ELSEIF(ISMSTR<=4.AND.ISMSTR/=3.AND.JLAG>0)THEN
        DO I=LFT,LLT
         IF(OFFG(I)>ONE)THEN
C-----------------------------------------------
          DO N=1,10
            X=SAV(I,N)
            Y=SAV(I,N+10)
            Z=SAV(I,N+20)
            SAV(I,N)	= X - Y*WZZ(I) + Z*WYY(I)
            SAV(I,N+10) = Y - Z*WXX(I) + X*WZZ(I)
            SAV(I,N+20) = Z - X*WYY(I) + Y*WXX(I) 
          END DO
         ENDIF
        END DO
      ELSEIF(ISMSTR==11)THEN
        DO I=LFT,LLT
         IF(OFFG(I)>ZERO) THEN
          DO N=1,10
            X=SAV(I,N)
            Y=SAV(I,N+10)
            Z=SAV(I,N+20)
            SAV(I,N)	= X - Y*WZZ(I) + Z*WYY(I)
            SAV(I,N+10) = Y - Z*WXX(I) + X*WZZ(I)
            SAV(I,N+20) = Z - X*WYY(I) + Y*WXX(I) 
          END DO
         END IF!(OFFG(I)>ZERO) THEN
        END DO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  S10MALLGEO3                   source/elements/solid/solide10/s10mall3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|        SMALLGEO3                     source/elements/solid/solide/small3.F
Chd|====================================================================
      SUBROUTINE S10MALLGEO3(NGL, OFFG ,VOLN ,DELTAX, VOLG0 ,
     .                       RX , RY , RZ , 
     .                       SX , SY , SZ ,
     .                       TX , TY , TZ )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(*)
C     REAL
      my_real
     .   OFFG(*),VOLN(*),DELTAX(*), VOLG0(*),
     .   RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*),TX(*), TY(*), TZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITET
      my_real
     .   A1,A2,A3,A4,
     .   A1X,A2X,A3X,A4X,A1Y,A2Y,A3Y,A4Y,A1Z,A2Z,A3Z,A4Z,
     .   LC(MVSIZ),VOLG(MVSIZ)
C---------------------------------------------
      IF(ISMSTR==1.OR.ISMSTR==3.OR.ISMSTR==11) RETURN
      IF(MIN_ASPECT==ZERO.AND.MIN_DEFV==ZERO) RETURN
C      
        DO I=LFT,LLT
         VOLG(I) = VOLN(I)*NPT
        END DO
      IF(ISROT == 0)THEN
        DO I=LFT,LLT
          A1X = RY(I)*SZ(I)-RZ(I)*SY(I)
          A1Y = RZ(I)*SX(I)-RX(I)*SZ(I)
          A1Z = RX(I)*SY(I)-RY(I)*SX(I)
          A1 = A1X*A1X+A1Y*A1Y+A1Z*A1Z
      
          A2X = SY(I)*TZ(I)-SZ(I)*TY(I)
          A2Y = SZ(I)*TX(I)-SX(I)*TZ(I)
          A2Z = SX(I)*TY(I)-SY(I)*TX(I)
          A2 = A2X*A2X+A2Y*A2Y+A2Z*A2Z
      
          A3X = TY(I)*RZ(I)-TZ(I)*RY(I)
          A3Y = TZ(I)*RX(I)-TX(I)*RZ(I)
          A3Z = TX(I)*RY(I)-TY(I)*RX(I)
          A3 = A3X*A3X+A3Y*A3Y+A3Z*A3Z
      
          A4X = A1X+A2X+A3X
          A4Y = A1Y+A2Y+A3Y
          A4Z = A1Z+A2Z+A3Z
          A4 = A4X*A4X+A4Y*A4Y+A4Z*A4Z
      
          LC(I) = SIX*VOLG(I)/SQRT(MAX(A1,A2,A3,A4))
        END DO
      ELSE
        DO I=LFT,LLT
         LC(I) = DELTAX(I)
        END DO
      END IF ! (ISROT == 0)THEN
C
      ITET = 1
      CALL SMALLGEO3(NGL ,OFFG ,VOLG ,LC, VOLG0 ,ITET )
C      
      RETURN
      END
