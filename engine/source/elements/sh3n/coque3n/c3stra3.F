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
Chd|  C3STRA3                       source/elements/sh3n/coque3n/c3stra3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3STRA3(JFT   ,JLT     ,PM     ,
     2                   MAT   ,AREA    ,EXX    ,EYY    ,EXY  ,
     3                   EXZ   ,EYZ     ,KXX    ,KYY    ,KXY  , 
     4                   GEO   ,PID     ,NU     ,SHF    ,GSTR ,
     5                   SSP   ,RHO     ,EPSDOT ,
     6                   NFT   ,ISTRAIN ,ISMSTR ,
     7                   UX1   ,UX2     ,UX3    ,UY1     ,UY2 ,
     8                   UY3   ,PX1     ,PY1    ,PY2     ,MTN ,
     9                   F_DEF ,WXY     ,GSTRW  ,NEL     )
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
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NFT,ISTRAIN,ISMSTR,NEL
      INTEGER MAT(MVSIZ),PID(MVSIZ),MTN
C     REAL ou REAL*8
      my_real
     .   GSTR(NEL,8), PM(NPROPM,*),GEO(NPROPG,*),
     .   AREA(MVSIZ),SHF(MVSIZ),
     .   EXX(MVSIZ), EYY(MVSIZ), EXY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ),
     .   KXX(MVSIZ), KYY(MVSIZ), KXY(MVSIZ),NU(MVSIZ),SSP(*),RHO(*),
     .   EPSDOT(6,*),
     .   UX1(*),UX2(*),UX3(*),UY1(*),UY2(*),UY3(*),
     .   PX1(*),PY1(*),PY2(*),WXY(*),F_DEF(MVSIZ,*),GSTRW(*)
      my_real
     .   UX12,UX13,UX23,UY13,UY23,EXXT,EYYT,EXYT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISH(MVSIZ),
     .        I, MX, J
C     REAL ou REAL*8
      my_real
     .   FSH(MVSIZ),
     .   THK2 ,FAC1, FAC2, FC1P
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
      IF(IEPSDOT.NE.0)THEN
        DO I=JFT,JLT
          FAC1  =UN/AREA(I)
          J = I + NFT
          EPSDOT(1,J)=EXX(I)*FAC1
          EPSDOT(2,J)=EYY(I)*FAC1
          EPSDOT(3,J)=EXY(I)*FAC1
          EPSDOT(4,J)=KXX(I)*FAC1
          EPSDOT(5,J)=KYY(I)*FAC1
          EPSDOT(6,J)=KXY(I)*FAC1
        ENDDO
      ENDIF
C
      DO I=JFT,JLT
        FAC1  =DT1/AREA(I)
        EXX(I)=EXX(I)*FAC1
        EYY(I)=EYY(I)*FAC1
        EXY(I)=EXY(I)*FAC1
        EYZ(I)=EYZ(I)*FAC1
        EXZ(I)=EXZ(I)*FAC1
        KXX(I)=KXX(I)*FAC1
        KYY(I)=KYY(I)*FAC1
        KXY(I)=KXY(I)*FAC1
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
         GSTRW(I)=GSTRW(I)+WXY(I)*DT1/AREA(I)
        ENDDO
C-----    
        DO I=JFT,JLT 
          F_DEF(I,6) = GSTR(I,6)
          F_DEF(I,7) = GSTR(I,7)
          F_DEF(I,8) = (GSTR(I,8)+GSTRW(I))*UNDEMI
          F_DEF(I,5) = (GSTR(I,8)-GSTRW(I))*UNDEMI
        ENDDO
       ELSEIF (ISMSTR /= 11 ) THEN
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
          FAC1  =UN/AREA(I)
          UX12=UX1(I)-UX2(I)
          UX13=UX1(I)-UX3(I)
          UX23=UX2(I)-UX3(I)
          UY13=UY1(I)-UY3(I)
          UY23=UY2(I)-UY3(I)
          EXXT= PX1(I)*UX12*FAC1
          EYYT=(PY1(I)*UY13+PY2(I)*UY23)*FAC1
          EXYT=(PY1(I)*UX13+PY2(I)*UX23)*FAC1
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
C-----------
      RETURN
      END
