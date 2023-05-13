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
Chd|  S10MALLGEO3                   source/elements/solid/solide10/s10mallgeo3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|====================================================================
      SUBROUTINE S10MALLGEO3(
     1   NGL,     OFFG,    VOLG,    DELTAX,
     2   VOLG0,   RX,      RY,      RZ,
     3   SX,      SY,      SZ,      TX,
     4   TY,      TZ,      LC,     GEO,    
     5   NEL,     NPT, ISMSTR,  ISROT,DT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NPT
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: ISROT
      INTEGER NGL(*)
C     REAL
      my_real, DIMENSION(NPROPG) , INTENT(IN)  :: GEO 
      my_real, DIMENSION(MVSIZ) , INTENT(OUT)  :: LC 
      my_real
     .   OFFG(*),VOLG(*),DELTAX(*), VOLG0(*),
     .   RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*),TX(*), TY(*), TZ(*)
      TYPE(DT_), INTENT(INOUT) :: DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ITET,IDELM,ICST
      my_real
     .   A1,A2,A3,A4,
     .   A1X,A2X,A3X,A4X,A1Y,A2Y,A3Y,A4Y,A1Z,A2Z,A3Z,A4Z,
     .   VDEFMIN,VDEFMAX,ASPMIN,ASPTET,MIN_ASPECT,MIN_DEFV,DELM
C---------------------------------------------
      MIN_ASPECT = DT%BRICK_CST_COL_MIN
      MIN_DEFV = DT%BRICK_CST_DEFV_MIN
      VDEFMIN = GEO(190)
      VDEFMAX = GEO(191)
      ASPMIN  = GEO(192)
      ASPTET  = GEO(193)
      DELM = VDEFMIN +VDEFMAX+ASPMIN+ASPTET
      ICST = 1
      IF((MIN_ASPECT+MIN_DEFV)==ZERO.AND.
     .   (ISMSTR==1.OR.ISMSTR==3.OR.ISMSTR==11)) ICST=0
      IDELM = 1
      IDELM = DT%IDEL_BRICK
      IF(IDELM ==0.AND.DELM==ZERO) IDELM = 0
      IF((IDELM+ICST) >0) THEN
        IF(ISROT == 0)THEN
          DO I=1,NEL
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
          DO I=1,NEL
           LC(I) = DELTAX(I)
          END DO
        END IF ! (ISROT == 0)THEN
      END IF ! ((IDELM+ICST) >0) THEN
C      
      RETURN
      END
