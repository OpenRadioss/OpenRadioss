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
Chd|  CDKEPSINI                     source/elements/sh3n/coquedk/cdkepsini.F
Chd|-- called by -----------
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- calls ---------------
Chd|        CDKDEFOI                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CDKEVEC3                      source/elements/sh3n/coquedk/cdkevec3.F
Chd|        CDKPXPYI                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CMLAWI                        source/elements/shell/coque/cepsini.F
Chd|        CORTH3                        source/elements/shell/coque/corth3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CDKEPSINI(ELBUF_STR,
     1           JFT  ,JLT   ,ISMSTR,ILAW  ,ITHK   ,
     2           PM   ,GEO   ,IXTG  ,X    ,XREFTG ,
     3           FOR  ,THK   ,EINT  ,GSTR ,
     4           PX2G ,PY2G  ,PX3G  ,PY3G ,X2S    ,
     5           Y2S  ,X3S   ,Y3S   ,OFF  ,IMAT   ,
     6           UVAR ,UPARAM,IPM   ,IGEO ,NEL    ,
     7           NLAY ,DIR_A ,DIR_B ,SIGI ,NPF    ,
     8           TF   ,IREP  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
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
#include      "param_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, ISMSTR, ILAW,IMAT,ITHK,NEL,IREP,NLAY
      INTEGER IXTG(NIXTG,*),IGEO(NPROPGI,*),IPM(NPROPMI,*),NPF(*)
      my_real
     . PM(NPROPM,*),GEO(NPROPG,*)   ,X(3,*)   ,XREFTG(3,3,*),
     . FOR(NEL,5) ,THK(*)   ,EINT(JLT,2),GSTR(NEL,8),
     . PX2G(*)  ,PY2G(*)  ,PX3G(*)  ,PY3G(*) ,UPARAM(*),UVAR(*),
     . X2S(*)   ,X3S(*)   ,Y2S(*)   ,Y3S(*) ,OFF(*),
     . DIR_A(NEL,*),DIR_B(NEL,*),SIGI(NEL,3),TF(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NT, I1, I2, I3
      my_real FAC
      my_real
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   X1(MVSIZ) , X2(MVSIZ) , X3(MVSIZ) ,X4(MVSIZ) ,
     .   Y1(MVSIZ) , Y2(MVSIZ) , Y3(MVSIZ) ,Y4(MVSIZ) ,
     .   Z1(MVSIZ) , Z2(MVSIZ) , Z3(MVSIZ), Z4(MVSIZ) ,AREA(MVSIZ),
     .   PX2(MVSIZ), PX3(MVSIZ), PY2(MVSIZ), PY3(MVSIZ),
     .   EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EYZ(MVSIZ),EZX(MVSIZ),
     .   VL(3,3,MVSIZ), XL(3,3,MVSIZ)
      my_real, 
     .   DIMENSION(:) , POINTER :: DIR1, DIR2
C=======================================================================
      DO I=JFT,JLT
        I1 = IXTG(2,I)
        I2 = IXTG(3,I)
        I3 = IXTG(4,I)
        X1(I) = ZERO
        Y1(I) = ZERO
        Z1(I) = ZERO
        X2(I) = X(1,I2)-X(1,I1)
        Y2(I) = X(2,I2)-X(2,I1)
        Z2(I) = X(3,I2)-X(3,I1)
        X3(I) = X(1,I3)-X(1,I1)
        Y3(I) = X(2,I3)-X(2,I1)
        Z3(I) = X(3,I3)-X(3,I1)
      ENDDO
c
      CALL CDKEVEC3(JFT ,JLT ,AREA,
     .              X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .              Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .              E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
c
      DO I=JFT,JLT
        XL(1,2,I) = E1X(I)*X2(I) + E1Y(I)*Y2(I) + E1Z(I)*Z2(I)
        XL(2,2,I) = E2X(I)*X2(I) + E2Y(I)*Y2(I) + E2Z(I)*Z2(I)
        XL(1,3,I) = E1X(I)*X3(I) + E1Y(I)*Y3(I) + E1Z(I)*Z3(I)
        XL(2,3,I) = E2X(I)*X3(I) + E2Y(I)*Y3(I) + E2Z(I)*Z3(I)
      ENDDO
c
      DO I=JFT,JLT
        X2(I) = XREFTG(2,1,I) - XREFTG(1,1,I)
        Y2(I) = XREFTG(2,2,I) - XREFTG(1,2,I)
        Z2(I) = XREFTG(2,3,I) - XREFTG(1,3,I)
        X3(I) = XREFTG(3,1,I) - XREFTG(1,1,I)
        Y3(I) = XREFTG(3,2,I) - XREFTG(1,2,I)
        Z3(I) = XREFTG(3,3,I) - XREFTG(1,3,I)
      ENDDO
c
      CALL CDKEVEC3(JFT ,JLT ,AREA,
     .              X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .              Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .              E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
c
      DO I=JFT,JLT
        VL(1,1,I) = ZERO
        VL(2,1,I) = ZERO
        VL(3,1,I) = ZERO
        VL(1,2,I) = E1X(I)*XL(1,2,I)+E2X(I)*XL(2,2,I)
        VL(2,2,I) = E1Y(I)*XL(1,2,I)+E2Y(I)*XL(2,2,I)
        VL(3,2,I) = E1Z(I)*XL(1,2,I)+E2Z(I)*XL(2,2,I)
        VL(1,3,I) = E1X(I)*XL(1,3,I)+E2X(I)*XL(2,3,I)
        VL(2,3,I) = E1Y(I)*XL(1,3,I)+E2Y(I)*XL(2,3,I)
        VL(3,3,I) = E1Z(I)*XL(1,3,I)+E2Z(I)*XL(2,3,I)
      ENDDO
c
      FAC = ONE / FLOAT(NITRS)
      DO I=JFT,JLT
        VL(1,2,I) = (VL(1,2,I) - X2(I))*FAC
        VL(2,2,I) = (VL(2,2,I) - Y2(I))*FAC
        VL(3,2,I) = (VL(3,2,I) - Z2(I))*FAC
        VL(1,3,I) = (VL(1,3,I) - X3(I))*FAC
        VL(2,3,I) = (VL(2,3,I) - Y3(I))*FAC
        VL(3,3,I) = (VL(3,3,I) - Z3(I))*FAC
      ENDDO
c--------------------------------------------------------
      IF (ISMSTR/=1) THEN
        DO NT=1,NITRS
          FAC = FLOAT(NT) - ONE
C
          DO I=JFT,JLT
             X1(I) = XREFTG(1,1,I) + FAC*VL(1,1,I)
             Y1(I) = XREFTG(1,2,I) + FAC*VL(2,1,I)
             Z1(I) = XREFTG(1,3,I) + FAC*VL(3,1,I)
             X2(I) = XREFTG(2,1,I) + FAC*VL(1,2,I)
             Y2(I) = XREFTG(2,2,I) + FAC*VL(2,2,I)
             Z2(I) = XREFTG(2,3,I) + FAC*VL(3,2,I)
             X3(I) = XREFTG(3,1,I) + FAC*VL(1,3,I)
             Y3(I) = XREFTG(3,2,I) + FAC*VL(2,3,I)
             Z3(I) = XREFTG(3,3,I) + FAC*VL(3,3,I)
          ENDDO
c
          CALL CDKEVEC3(JFT ,JLT ,AREA,
     .                  X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                  Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .                  E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
c
          CALL CORTH3(ELBUF_STR,DIR_A   ,DIR_B   ,JFT    ,JLT    ,
     .                NLAY     ,IREP    ,NEL     ,
     .                X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
c
          CALL CDKPXPYI(JFT  ,JLT  ,ISMSTR,
     .                  PX2G ,PY2G ,PX3G ,PY3G ,
     .                  PX2  ,PY2  ,PX3  ,PY3  ,
     .                  X2S  ,Y2S  ,X3S  ,Y3S  ,
     .                  X1   ,X2   ,X3   ,Y1   ,Y2   ,
     .                  Y3   ,Z1   ,Z2   ,Z3   ,
     .                  E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,
     .                  E3Y  ,E1Z  ,E2Z  ,E3Z  ,AREA )
c
          CALL CDKDEFOI(JFT  ,JLT  ,NEL  ,VL   ,GSTR ,
     .                  PX2  ,PY2  ,PX3  ,PY3  ,
     .                  E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,
     .                  E3Y  ,E1Z  ,E2Z  ,E3Z  ,
     .                  EXX  ,EYY  ,EXY  ,EYZ  ,EZX  )
c
          CALL CMLAWI(JFT  ,JLT  ,ILAW    ,ITHK ,PM   ,
     .                FOR  ,THK  ,EINT   ,GSTR ,DIR_A,
     .                DIR_B,UVAR ,UPARAM,IPM   ,IGEO ,
     .                NEL  ,SIGI ,ISMSTR,NPF   ,TF   ,
     .                AREA ,EXX  ,EYY   ,EXY   ,IMAT )
        ENDDO
      ELSE
C------
C       SMALL STRAIN FORMULATION.
C------
        CALL CDKPXPYI(JFT  ,JLT  ,ISMSTR,
     .              PX2G ,PY2G ,PX3G ,PY3G ,
     .              PX2  ,PY2  ,PX3  ,PY3  ,
     .              X2S  ,Y2S  ,X3S  ,Y3S  ,
     .              X1   ,X2   ,X3   ,Y1   ,Y2   ,
     .              Y3   ,Z1   ,Z2   ,Z3   ,
     .              E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,
     .              E3Y  ,E1Z  ,E2Z  ,E3Z  ,AREA )
        DIR1 => ELBUF_STR%BUFLY(1)%DIRA
        DIR2 => ELBUF_STR%BUFLY(1)%DIRB
c
        DO NT=1,NITRS
          CALL CDKDEFOI(JFT  ,JLT  ,NEL  ,VL   ,GSTR ,
     .                  PX2  ,PY2  ,PX3  ,PY3  ,
     .                  E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,
     .                  E3Y  ,E1Z  ,E2Z  ,E3Z  ,
     .                  EXX  ,EYY  ,EXY  ,EYZ  ,EZX  )

          CALL CMLAWI(JFT  ,JLT  ,ILAW    ,ITHK ,PM   ,
     .                FOR  ,THK  ,EINT   ,GSTR ,DIR_A,
     .                DIR_B,UVAR ,UPARAM,IPM   ,IGEO ,
     .                NEL  ,SIGI ,ISMSTR,NPF   ,TF   ,
     .                AREA ,EXX  ,EYY   ,EXY   ,IMAT )
c
        ENDDO
        OFF(JFT:JLT) = TWO
      ENDIF
C ---
      RETURN
      END
