Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  CZSTRA3                       source/elements/shell/coquez/czstra3.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CZSTRA3(JFT    ,JLT    ,NFT   ,VDEF  ,GSTR  ,
     2                   EXX    ,EYY    ,EXY   ,EXZ   ,EYZ   ,  
     3                   KXX    ,KYY    ,KXY   ,DT1C  ,EPSDOT,
     4                   IEPSDOT,ISTRAIN,UX1   ,UX2   ,UX3   ,
     5                   UX4    ,UY1    ,UY2   ,UY3   ,UY4   ,
     6                   PX1    ,PX2    ,PY1   ,PY2   ,AREA  ,
     7                   ISMSTR ,MTN    ,WXY   ,F_DEF ,GSTRW ,
     8                   NEL    )
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
      INTEGER JFT, JLT, NFT, IEPSDOT, ISTRAIN, ISMSTR, MTN,NEL
      my_real
     .   VDEF(MVSIZ,8), GSTR(NEL,8),
     .   EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EXZ(MVSIZ),EYZ(MVSIZ),
     .   KXX(MVSIZ),KYY(MVSIZ),KXY(MVSIZ),DT1C(MVSIZ),EPSDOT(6,*),
     .   UX1(*),UX2(*),UX3(*),UX4(*),UY1(*),UY2(*),UY3(*),UY4(*),
     .   PX1(*),PX2(*),PY1(*),PY2(*),AREA(*),F_DEF(MVSIZ,8),WXY(*),GSTRW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .        FAC1(MVSIZ),UX13,UX24,UY13,UY24,
     .        EXXT,EYYT,EXYT
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
      IF(IEPSDOT/=0)THEN
       DO I=JFT,JLT
        J = I + NFT
        EPSDOT(1,J) = VDEF(I,1)
        EPSDOT(2,J) = VDEF(I,2)
        EPSDOT(3,J) = VDEF(I,3)
        EPSDOT(4,J) = VDEF(I,6)
        EPSDOT(5,J) = VDEF(I,7)
        EPSDOT(6,J) = VDEF(I,8)
       ENDDO
      ENDIF
C
      DO I=JFT,JLT
        EXX(I) = VDEF(I,1) * DT1C(I)
        EYY(I) = VDEF(I,2) * DT1C(I)
        EXY(I) = VDEF(I,3) * DT1C(I)
        EYZ(I) = VDEF(I,5) * DT1C(I)
        EXZ(I) = VDEF(I,4) * DT1C(I)
        KXX(I) = VDEF(I,6) * DT1C(I)
        KYY(I) = VDEF(I,7) * DT1C(I)
        KXY(I) = VDEF(I,8) * DT1C(I)
      ENDDO
C
      IF (ISTRAIN /= 0.OR.ISMSTR == 10) THEN
       IF(ISMSTR == 10)THEN
        DO I=JFT,JLT
         GSTR(I,1)=GSTR(I,1)+EXX(I)
         GSTR(I,2)=GSTR(I,2)+EYY(I)
         GSTR(I,3)=GSTR(I,3)+EXY(I)
         GSTR(I,4)=GSTR(I,4)+EYZ(I)
         GSTR(I,5)=GSTR(I,5)+EXZ(I)
         GSTR(I,6)=GSTR(I,6)+KXX(I)
         GSTR(I,7)=GSTR(I,7)+KYY(I)
         GSTR(I,8)=GSTR(I,8)+KXY(I)
C-------- WXY         
         GSTRW(I)=GSTRW(I)+WXY(I)*DT1C(I)
        ENDDO
C-----   
        DO I=JFT,JLT 
          F_DEF(I,6) = GSTR(I,6)
          F_DEF(I,7) = GSTR(I,7)
          F_DEF(I,8) = (GSTR(I,8)+GSTRW(I))*HALF
          F_DEF(I,5) = (GSTR(I,8)-GSTRW(I))*HALF
        ENDDO
       ELSEIF(ISMSTR /= 11)THEN
        DO I=JFT,JLT
         GSTR(I,1)=GSTR(I,1)+EXX(I)
         GSTR(I,2)=GSTR(I,2)+EYY(I)
         GSTR(I,3)=GSTR(I,3)+EXY(I)
         GSTR(I,4)=GSTR(I,4)+EYZ(I)
         GSTR(I,5)=GSTR(I,5)+EXZ(I)
         GSTR(I,6)=GSTR(I,6)+KXX(I)
         GSTR(I,7)=GSTR(I,7)+KYY(I)
         GSTR(I,8)=GSTR(I,8)+KXY(I)
        ENDDO
       ELSE
        DO I=JFT,JLT
         FAC1(I)  =ONE/AREA(I)
         UX13=UX1(I)-UX3(I)
         UX24=UX2(I)-UX4(I)
         UY13=UY1(I)-UY3(I)
         UY24=UY2(I)-UY4(I)
         EXXT=(PX1(I)*UX13+PX2(I)*UX24)*FAC1(I)
         EYYT=(PY1(I)*UY13+PY2(I)*UY24)*FAC1(I)
         EXYT=(PY1(I)*UX13+PY2(I)*UX24
     .        +PX1(I)*UY13+PX2(I)*UY24)*FAC1(I)
         GSTR(I,1)=EXXT
         GSTR(I,2)=EYYT
         GSTR(I,3)=EXYT
         GSTR(I,4)=GSTR(I,4)+EYZ(I)
         GSTR(I,5)=GSTR(I,5)+EXZ(I)
         GSTR(I,6)=GSTR(I,6)+KXX(I)
         GSTR(I,7)=GSTR(I,7)+KYY(I)
         GSTR(I,8)=GSTR(I,8)+KXY(I)
        ENDDO
       ENDIF
      ENDIF
C
      RETURN
      END
