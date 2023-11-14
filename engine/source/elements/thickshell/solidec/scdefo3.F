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
Chd|  SCDEFO3                       source/elements/thickshell/solidec/scdefo3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCDEFO3(
     1   DXX,     DXY,     DXZ,     DYX,
     2   DYY,     DYZ,     DZX,     DZY,
     3   DZZ,     D4,      D5,      D6,
     4   DCXX,    DCXY,    DCXZ,    DCYX,
     5   DCYY,    DCYZ,    DCZX,    DCZY,
     6   DCZZ,    DC4,     DC5,     DC6,
     7   DHXX,    DHXY,    DHXZ,    DHYX,
     8   DHYY,    DHYZ,    DHZX,    DHZY,
     9   DHZZ,    DH4,     DH5,     DH6,
     A   ZI,      WI,      VZL,     VOL,
     B   VOLG,    VOLO,    DDHV,    SIG,
     C   SIGZM,   VOLM,    USB,     EINT,
     D   OFF,     OFFG,    DTI,     OFFS,
     E   DVC,     VOL0DP,  VOLDP,   IPRES,
     F   NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,IPRES
C     REAL
      my_real
     .   DXX(*), DXY(*), DXZ(*), DYX(*), DYY(*),DYZ(*), 
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   DCXX(*), DCXY(*),DCXZ(*), DCYX(*), DCYY(*),DCYZ(*), 
     .   DCZX(*), DCZY(*), DCZZ(*), DC4(*), DC5(*), DC6(*),
     .   DHXX(*), DHXY(*),DHXZ(*), DHYX(*), DHYY(*),DHYZ(*), 
     .   DHZX(*), DHZY(*), DHZZ(*), 
     .   DH4(*), DH5(*), DH6(*), DDHV(*),
     .   ZI,WI,VZL(*),VOL(*),VOLG(*),VOLO(*),
     .   SIG(NEL,6),SIGZM(*),VOLM(*),USB(*),EINT(*),OFF(*),OFFG(*),DTI,
     .   OFFS(*),DVC(*)
      DOUBLE PRECISION 
     .   VOL0DP(*),VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
      my_real
     .  DV,DVZ,DDV,TOL,DT1D2,DT1D1
C=======================================================================
      TOL=ONE-EM20
      DO I=1,NEL
        VOLDP(I)=HALF*WI*(VOLG(I)+VZL(I)*ZI)
        VOL(I)=VOLDP(I)
        OFF(I)=OFFG(I)
        IF (VOL(I)<=ZERO) THEN
           VOL(I)= EM20
           OFF(I)= ZERO
        ELSEIF(OFF(I)==ZERO.OR.OFFS(I)==TWO.OR.ISMDISP>0)THEN
           VOLDP(I)=MAX(EM20,VOLDP(I))
           VOL(I)=MAX(EM20,VOL(I))
        ENDIF
      ENDDO
      IF (IPRES==1) THEN
        DO I=1,NEL
C  ------due to the assumed strain terms----
          DV = DDHV(I)*DT1*ZI
C  ------due to the sigzz traitement----
          DVZ = -(SIG(I,3)-SIGZM(I)/MAX(EM20,VOLM(I)))*USB(I)
          DDV = (DV+DVZ)*OFF(I)
          DVC(I) = DDV
          IF (DDV>TOL) THEN
           VOL(I)= EM20
           OFF(I)= ZERO
           DDV = ZERO
          ELSEIF(OFF(I)==ZERO.OR.OFFS(I)==TWO.OR.ISMDISP>0)THEN
           DDV = ZERO
          END IF
C         
          IF (IRESP==1) VOL0DP(I) = VOL0DP(I)*(ONE- DDV)
          IF(ISMDISP>0.OR.OFFS(I)==TWO) DVC(I) = DVZ
C      
          VOLO(I) = VOLO(I)*(ONE- DDV)
          EINT(I) = EINT(I)/(ONE- DDV)
        ENDDO
      ELSE
        DO I=1,NEL
          DVC(I) = ZERO
        ENDDO
      END IF
C   +++ -----partie non constante------
      DO I=1,NEL
        DXX(I)=DCXX(I)+ZI*DHXX(I)
        DYY(I)=DCYY(I)+ZI*DHYY(I)
        DZZ(I)=DCZZ(I)+ZI*DHZZ(I)
        DXY(I)=DCXY(I)+ZI*DHXY(I)
        DYX(I)=DCYX(I)+ZI*DHYX(I)
        DZX(I)=DCZX(I)+ZI*DHZX(I)
        DZY(I)=DCZY(I)+ZI*DHZY(I)
        DXZ(I)=DCXZ(I)+ZI*DHXZ(I)
        DYZ(I)=DCYZ(I)+ZI*DHYZ(I)
c        D4(I)=DC4(I)+ZI*DH4(I)
c        D5(I)=DC5(I)+ZI*DH5(I)
c        D6(I)=DC6(I)+ZI*DH6(I)
      ENDDO
C-----------
      DT1D1=DT1
      IF (ISMDISP>0.AND.ISCAU==0) DT1D1=ZERO
      DT1D2=HALF*DT1D1
       DO I=1,NEL
        D4(I)   = DXY(I)+DYX(I)
     .           -DT1D1*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
        D5(I)   = DYZ(I)+DZY(I)
     .           -DT1D1*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
        D6(I)   = DXZ(I)+DZX(I)
     .           -DT1D1*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
        DXX(I)  = DXX(I)
     .           -DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
        DYY(I)  = DYY(I)
     .           -DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
        DZZ(I)  = DZZ(I)
     .           -DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
       ENDDO
C       
      RETURN
      END
